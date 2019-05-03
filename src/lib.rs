use std::{io, path::Path};

use failure::Fail;

use pchomp::{ParseError, Parser};

#[derive(Debug, Fail)]
#[fail(
    display = "Error in file {:?} at {}:{}: `{}`",
    filename, line, col, kind
)]
pub struct JavaParseError {
    pub line: usize,
    pub col: usize,
    pub byte: usize,
    pub filename: Option<String>,
    pub kind: JavaParseErrorKind,
}

#[derive(Debug, Fail)]
#[fail(display = "Internal error type")]
pub struct InnerParseError {
    byte: usize,
    kind: JavaParseErrorKind,
}

#[derive(Debug, Fail)]
pub enum JavaParseErrorKind {
    #[fail(display = "Parse error")]
    ParseError(#[cause] ParseError),

    #[fail(display = "Unknown error")]
    UnknownError,
}

impl From<ParseError> for InnerParseError {
    fn from(e: ParseError) -> InnerParseError {
        InnerParseError {
            byte: e.byte,
            kind: JavaParseErrorKind::ParseError(e),
        }
    }
}

pub type Ident<'a> = &'a [u8];

#[derive(Debug, Default, Clone, PartialEq)]
pub struct Scoped<'a>(pub Vec<Ident<'a>>);

#[derive(Debug, Default, Clone, PartialEq)]
pub struct SymbolSoup<'a> {
    pub idents: Vec<Ident<'a>>,
}

#[derive(Debug, PartialEq)]
pub struct Parse<'a> {
    pub source: &'a str,
    pub package: Scoped<'a>,
    pub imports: Vec<Import<'a>>,
    pub import_span: [usize; 2],
    pub classes: Vec<Class<'a>>,
}

#[derive(Debug, Default, PartialEq)]
pub struct Import<'a> {
    pub path: Scoped<'a>,
    pub is_static: bool,
    pub star: bool,
}

#[derive(Debug, Default, Clone, PartialEq)]
pub struct Class<'a> {
    pub annotations: Vec<Type<'a>>,
    pub name: Ident<'a>,
    pub fields: Vec<Field<'a>>,
    pub methods: Vec<Method<'a>>,
    pub variants: Vec<Ident<'a>>,
    pub type_params: Vec<Ident<'a>>,
    pub subtypes: Vec<Type<'a>>,
}

#[derive(Debug, Default, Clone, PartialEq)]
pub struct Type<'a> {
    pub type_name: Scoped<'a>,
    pub type_params: Vec<Type<'a>>,
}

#[derive(Debug, Default, Clone, PartialEq)]
pub struct Field<'a> {
    pub annotations: Vec<Type<'a>>,
    pub name: Ident<'a>,
    pub field_type: Type<'a>,
    pub value: SymbolSoup<'a>,
}

#[derive(Debug, Default, Clone, PartialEq)]
pub struct Method<'a> {
    pub annotations: Vec<Type<'a>>,
    pub type_params: Vec<Ident<'a>>,
    pub return_type: Type<'a>,
    pub name: Ident<'a>,
    pub args: Vec<Arg<'a>>,
    pub throws: Vec<Type<'a>>,
}

#[derive(Debug, Default, Clone, PartialEq)]
pub struct Arg<'a> {
    pub annotations: Vec<Type<'a>>,
    pub name: Ident<'a>,
    pub arg_type: Type<'a>,
}

pub struct Project {
    files: Vec<(String, String)>,
}

pub fn read_parent_project<P: AsRef<Path>>(filename: P) -> io::Result<Project> {
    let path = filename.as_ref();
    let mut path = Some(path);
    while let Some(name) = path.and_then(Path::file_name) {
        match name == "src" {
            true => break,
            false => path = path.unwrap().parent(),
        }
    }

    let src_path = path.ok_or_else(|| {
        io::Error::new(
            io::ErrorKind::NotFound,
            failure::err_msg("Source directory not found in parent tree"),
        )
    })?;
    read_project(&src_path)
}

pub fn read_project<P: AsRef<Path>>(dir: P) -> io::Result<Project> {
    use std::fs;

    let mut paths = vec![dir.as_ref().to_owned()];
    let mut files = vec![];

    while let Some(path) = paths.pop() {
        for entry in fs::read_dir(&path)? {
            let entry = entry?;
            match entry.file_type()? {
                t if t.is_dir() => paths.push(entry.path()),
                t if t.is_file() => {
                    let path = entry.path();
                    if path.extension() == Some("java".as_ref()) {
                        let name = path.display().to_string();
                        let content = fs::read_to_string(&path)?;
                        files.push((name, content));
                    }
                }
                _ => (),
            }
        }
    }

    Ok(Project { files })
}

pub fn parse_project<'a>(project: &'a Project) -> Result<Vec<Parse<'a>>, JavaParseError> {
    project
        .files
        .iter()
        .map(|(name, source)| parse_file(source, Some(name)))
        .collect()
}

pub fn parse_file<'a>(
    source: &'a str,
    filename: Option<&str>,
) -> Result<Parse<'a>, JavaParseError> {
    let convert_error = |e: InnerParseError| {
        let (line, col) = {
            let mut line = 1;
            let mut col = 0;
            for &b in &source.as_bytes()[..e.byte] {
                if b == b'\n' {
                    line += 1;
                    col = 0;
                }
            }
            (line, col + 1)
        };
        JavaParseError {
            line,
            col,
            byte: e.byte,
            filename: filename.map(str::to_owned),
            kind: e.kind,
        }
    };

    let parser = &mut Parser::new(source);
    parser.skip_whitespace();
    parser
        .expect_keyword(b"package")
        .map_err(|e| convert_error(e.into()))?;

    let package = parse_path(parser)
        .ok_or(InnerParseError {
            byte: parser.cursor(),
            kind: JavaParseErrorKind::UnknownError,
        })
        .map_err(convert_error)?;
    parser.expect(b";").map_err(|e| convert_error(e.into()))?;

    let import_start_point = parser.cursor();
    let imports = parse_imports(parser).map_err(convert_error)?;
    let import_end_point = parser.cursor();

    let mut classes = vec![];
    while !parser.finished() {
        classes.push(parse_class(parser).map_err(convert_error)?);
    }

    Ok(Parse {
        source,
        package,
        imports,
        import_span: [import_start_point, import_end_point],
        classes,
    })
}

fn parse_path<'a>(parser: &mut Parser<'a>) -> Option<Scoped<'a>> {
    let mut path = vec![];
    while let Some(ident) = parser.skip_ident() {
        path.push(ident);
        if !parser.skip(b".") {
            break;
        }
    }
    if path.is_empty() {
        None
    } else {
        Some(Scoped(path))
    }
}

fn parse_imports<'a>(parser: &mut Parser<'a>) -> Result<Vec<Import<'a>>, InnerParseError> {
    let mut imports = vec![];

    while parser.skip_keyword(b"import") {
        let is_static = parser.skip_keyword(b"static");
        let path = parse_path(parser).ok_or_else(|| InnerParseError {
            byte: parser.cursor(),
            kind: JavaParseErrorKind::UnknownError,
        })?;
        let star = parser.skip(b"*");

        imports.push(Import {
            path,
            is_static,
            star,
        });

        parser.expect(b";")?;
    }

    Ok(imports)
}

fn skip_visibility<'a>(parser: &mut Parser<'a>) -> bool {
    parser.skip_keyword(b"public")
        || parser.skip_keyword(b"protected")
        || parser.skip_keyword(b"private")
}

fn parse_class<'a>(parser: &mut Parser<'a>) -> Result<Class<'a>, InnerParseError> {
    skip_visibility(parser);

    while parser.skip(b"static") || parser.skip(b"final") || parser.skip(b"abstract") {}

    let is_enum = parser.skip_keyword(b"enum");
    let is_item = is_enum || parser.skip_keyword(b"class") || parser.skip_keyword(b"interface");

    if !is_item {
        return Err(InnerParseError {
            byte: parser.cursor(),
            kind: JavaParseErrorKind::UnknownError,
        });
    }

    let name = parser.expect_ident()?;

    let mut type_params = vec![];
    if parser.skip(b"<") {
        type_params = parse_comma_separated(parser, Parser::skip_ident);
        parser.expect(b">")?;
    }

    let mut subtypes = vec![];
    if parser.skip_keyword(b"extends") {
        subtypes.push(parse_type(parser)?.ok_or_else(|| InnerParseError {
            byte: parser.cursor(),
            kind: JavaParseErrorKind::UnknownError,
        })?);
    }
    if parser.skip_keyword(b"implements") {
        subtypes.append(&mut try_parse_comma_separated(parser, parse_type)?);
    }

    let mut fields = vec![];
    let mut methods = vec![];
    let mut variants = vec![];

    parser.expect(b"{")?;

    if is_enum {
        variants = parse_comma_separated(parser, Parser::skip_ident);
    }

    if !is_enum || parser.skip(b";") {
        while !parser.check(b"}") {
            skip_visibility(parser);

            while parser.skip(b"static") || parser.skip(b"final") {}

            let possible_constructor_name = parser.check_ident();
            let item_type = parse_type(parser)?.ok_or_else(|| InnerParseError {
                byte: parser.cursor(),
                kind: JavaParseErrorKind::UnknownError,
            })?;
            if parser.skip(b"(") {
                let args = try_parse_comma_separated(parser, parse_arg)?;
                parser.expect(b")")?;
                let throws = if parser.skip_keyword(b"throws") {
                    try_parse_comma_separated(parser, parse_type)?
                } else {
                    vec![]
                };
                parser.skip_around(b'{', b'}')?;

                let constructor = Method {
                    annotations: vec![],
                    type_params: vec![],
                    return_type: item_type,
                    name: possible_constructor_name.ok_or_else(|| InnerParseError {
                        byte: parser.cursor(),
                        kind: JavaParseErrorKind::UnknownError,
                    })?,
                    args,
                    throws,
                };
                methods.push(constructor);
            } else {
                let name = parser.expect_ident()?;
                if parser.skip(b"(") {
                    let args = try_parse_comma_separated(parser, parse_arg)?;
                    parser.expect(b")")?;
                    let throws = if parser.skip_keyword(b"throws") {
                        try_parse_comma_separated(parser, parse_type)?
                    } else {
                        vec![]
                    };
                    parser.skip_around(b'{', b'}')?;

                    let method = Method {
                        annotations: vec![],
                        type_params: vec![],
                        return_type: item_type,
                        name,
                        args,
                        throws,
                    };
                    methods.push(method);
                } else {
                    if parser.skip(b"=") {
                        let _expression_junk = parser.scan_to(b';');
                    }
                    let field = Field {
                        annotations: vec![],
                        field_type: item_type,
                        name,
                        value: SymbolSoup::default(),
                    };
                    fields.push(field);
                    parser.expect(b";")?;
                }
            }
        }
    }

    parser.skip_inside(b'{', b'}')?;
    parser.expect(b"}")?;

    Ok(Class {
        annotations: vec![],
        name,
        fields,
        methods,
        variants,
        type_params,
        subtypes,
    })
}

fn parse_type<'a>(parser: &mut Parser<'a>) -> Result<Option<Type<'a>>, InnerParseError> {
    match parse_path(parser) {
        Some(path) => {
            let mut type_params = vec![];

            if parser.skip(b"<") {
                type_params = try_parse_comma_separated(parser, parse_type)?;
                parser.expect(b">")?;
            }

            Ok(Some(Type {
                type_name: path,
                type_params,
            }))
        }
        None => Ok(None),
    }
}

fn parse_arg<'a>(parser: &mut Parser<'a>) -> Result<Option<Arg<'a>>, InnerParseError> {
    let arg_type = parse_type(parser)?;
    match arg_type {
        Some(arg_type) => {
            let name = parser.expect_ident()?;
            Ok(Some(Arg {
                annotations: vec![],
                arg_type,
                name,
            }))
        }
        None => Ok(None),
    }
}

fn parse_comma_separated<'a, T, F>(parser: &mut Parser<'a>, f: F) -> Vec<T>
where
    F: Fn(&mut Parser<'a>) -> Option<T>,
{
    let mut results = vec![];
    while let Some(item) = f(parser) {
        results.push(item);
        if !parser.skip(b",") {
            break;
        }
    }
    results
}

fn try_parse_comma_separated<'a, T, F>(
    parser: &mut Parser<'a>,
    f: F,
) -> Result<Vec<T>, InnerParseError>
where
    F: Fn(&mut Parser<'a>) -> Result<Option<T>, InnerParseError>,
{
    let mut results = vec![];
    while let Some(item) = f(parser)? {
        results.push(item);
        if !parser.skip(b",") {
            break;
        }
    }
    Ok(results)
}

#[cfg(test)]
mod tests {
    use super::*;

    macro_rules! testcase {
        ($name:ident, $input:literal, $fn:ident, $result:expr, $remainder:literal) => {
            #[test]
            fn $name() {
                let parser = &mut Parser::new($input);
                let result = $fn(parser);
                let remainder = parser.source()[parser.cursor()..].as_bytes();
                assert_eq!(result, $result);
                assert_eq!(remainder, $remainder.as_bytes());
            }
        };
    }

    macro_rules! testcase_ok {
        ($name:ident, $input:literal, $fn:ident, $result:expr, $remainder:literal) => {
            #[test]
            fn $name() {
                let parser = &mut Parser::new($input);
                let result = $fn(parser).unwrap();
                let remainder = parser.source()[parser.cursor()..].as_bytes();
                assert_eq!(result, $result);
                assert_eq!(remainder, $remainder.as_bytes());
            }
        };
    }

    macro_rules! testcase_err {
        ($name:ident, $input:literal, $fn:ident, $remainder:literal) => {
            #[test]
            fn $name() {
                let parser = &mut Parser::new($input);
                let result = $fn(parser).iter().next().is_some();
                let remainder = parser.source()[parser.cursor()..].as_bytes();
                assert!(!result);
                assert_eq!(remainder, $remainder.as_bytes());
            }
        };
    }

    testcase!(empty_path, "", parse_path, None, "");
    testcase!(
        path_of_one,
        "one",
        parse_path,
        Some(Scoped(vec![b"one"])),
        ""
    );
    testcase!(
        path_of_two,
        "one.Two",
        parse_path,
        Some(Scoped(vec![b"one", b"Two"])),
        ""
    );
    testcase!(
        path_trims,
        "one.Two  ;",
        parse_path,
        Some(Scoped(vec![b"one", b"Two"])),
        ";"
    );

    fn mkimport(path: &[&'static [u8]], is_static: bool, star: bool) -> Import<'static> {
        Import {
            path: Scoped(path.to_vec()),
            is_static,
            star,
        }
    }

    testcase_ok!(empty_imports, "", parse_imports, vec![], "");
    testcase_ok!(
        simple_import,
        "import utils;",
        parse_imports,
        vec![mkimport(&[b"utils"], false, false)],
        ""
    );
    testcase_ok!(
        two_import,
        "import utils; import tools;",
        parse_imports,
        vec![
            mkimport(&[b"utils"], false, false),
            mkimport(&[b"tools"], false, false),
        ],
        ""
    );
    testcase_ok!(
        static_import,
        "import static utils.func;",
        parse_imports,
        vec![mkimport(&[b"utils", b"func"], true, false)],
        ""
    );
    testcase_ok!(
        star_import,
        "import static utils.*;",
        parse_imports,
        vec![mkimport(&[b"utils"], true, true)],
        ""
    );

    fn mktype(path: &[&'static [u8]], params: &[Type<'static>]) -> Type<'static> {
        Type {
            type_name: Scoped(path.to_owned()),
            type_params: params.to_vec(),
        }
    }

    testcase_ok!(empty_type, "", parse_type, None, "");
    testcase_ok!(
        simple_type,
        "String",
        parse_type,
        Some(mktype(&[b"String"], &[])),
        ""
    );
    testcase_ok!(
        scoped_type,
        "java.util.String",
        parse_type,
        Some(mktype(&[b"java", b"util", b"String"], &[])),
        ""
    );
    testcase_ok!(
        generic_type,
        "List<T>",
        parse_type,
        Some(mktype(&[b"List"], &[mktype(&[b"T"], &[])])),
        ""
    );
    testcase_ok!(
        nested_generic_type,
        "List<List<package.Thing>>",
        parse_type,
        Some(mktype(
            &[b"List"],
            &[mktype(&[b"List"], &[mktype(&[b"package", b"Thing"], &[])])]
        )),
        ""
    );
    testcase_err!(malformed_type, "List<T", parse_type, "");

    testcase_ok!(
        empty_class,
        "public class Thing {}",
        parse_class,
        Class {
            name: b"Thing",
            ..Class::default()
        },
        ""
    );
    testcase_ok!(
        empty_qualified_class,
        "public static final class Thing {}",
        parse_class,
        Class {
            name: b"Thing",
            ..Class::default()
        },
        ""
    );
    testcase_ok!(
        simple_enum,
        "private enum Which { A, B, C }",
        parse_class,
        Class {
            name: b"Which",
            variants: vec![b"A", b"B", b"C"],
            ..Class::default()
        },
        ""
    );
    testcase_ok!(
        simple_enum_semi,
        "private enum Which { A, B, C ; }",
        parse_class,
        Class {
            name: b"Which",
            variants: vec![b"A", b"B", b"C"],
            ..Class::default()
        },
        ""
    );
    testcase_ok!(
        class_inheritance,
        "class Thing extends SuperThing implements Do, Re<Mi> {}",
        parse_class,
        Class {
            name: b"Thing",
            subtypes: vec![
                mktype(&[b"SuperThing"], &[]),
                mktype(&[b"Do"], &[]),
                mktype(&[b"Re"], &[mktype(&[b"Mi"], &[])]),
            ],
            ..Class::default()
        },
        ""
    );
    testcase_ok!(
        constructors,
        "class Thing { Thing(int size) {} }",
        parse_class,
        Class {
            name: b"Thing",
            methods: vec![Method {
                return_type: mktype(&[b"Thing"], &[]),
                name: b"Thing",
                args: vec![Arg {
                    arg_type: mktype(&[b"int"], &[]),
                    name: b"size",
                    ..Arg::default()
                }],
                ..Method::default()
            },],
            ..Class::default()
        },
        ""
    );
    testcase_ok!(
        methods,
        "class Thing { int reserve(int size) {} }",
        parse_class,
        Class {
            name: b"Thing",
            methods: vec![Method {
                return_type: mktype(&[b"int"], &[]),
                name: b"reserve",
                args: vec![Arg {
                    arg_type: mktype(&[b"int"], &[]),
                    name: b"size",
                    ..Arg::default()
                }],
                ..Method::default()
            },],
            ..Class::default()
        },
        ""
    );
    testcase_ok!(
        method_throws,
        "class Thing { int reserve(int size) throws IOException {} }",
        parse_class,
        Class {
            name: b"Thing",
            methods: vec![Method {
                return_type: mktype(&[b"int"], &[]),
                name: b"reserve",
                args: vec![Arg {
                    arg_type: mktype(&[b"int"], &[]),
                    name: b"size",
                    ..Arg::default()
                }],
                throws: vec![mktype(&[b"IOException"], &[])],
                ..Method::default()
            },],
            ..Class::default()
        },
        ""
    );
    testcase_ok!(
        fields,
        "class Thing { public int size; }",
        parse_class,
        Class {
            name: b"Thing",
            fields: vec![Field {
                field_type: mktype(&[b"int"], &[]),
                name: b"size",
                ..Field::default()
            }],
            ..Class::default()
        },
        ""
    );
    testcase_ok!(
        qualified_fields,
        "class Thing { public static final int size; }",
        parse_class,
        Class {
            name: b"Thing",
            fields: vec![Field {
                field_type: mktype(&[b"int"], &[]),
                name: b"size",
                ..Field::default()
            }],
            ..Class::default()
        },
        ""
    );
    testcase_ok!(
        field_with_expression,
        "class Thing { public int size = 100; }",
        parse_class,
        Class {
            name: b"Thing",
            fields: vec![Field {
                field_type: mktype(&[b"int"], &[]),
                name: b"size",
                ..Field::default()
            }],
            ..Class::default()
        },
        ""
    );

    #[test]
    fn parse_package() {
        let tests: &[(&str, &[&[u8]])] = &[
            ("package a;", &[b"a"]),
            ("package a.b.c;", &[b"a", b"b", b"c"]),
            (
                "package com.falseidolfactory.thing;",
                &[b"com", b"falseidolfactory", b"thing"],
            ),
        ];

        for (src, package) in tests {
            assert_eq!(parse_file(src).unwrap().package, Scoped(package.to_vec()));
        }
    }
}
