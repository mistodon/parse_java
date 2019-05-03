use failure::{self, bail, Error};

use pchomp::Parser;

pub type Ident<'a> = &'a [u8];

#[derive(Debug, Default, Clone, PartialEq)]
pub struct Path<'a>(pub Vec<Ident<'a>>);

#[derive(Debug, Default, Clone, PartialEq)]
pub struct SymbolSoup<'a> {
    pub idents: Vec<Ident<'a>>,
}

#[derive(Debug, PartialEq)]
pub struct Parse<'a> {
    pub source: &'a str,
    pub package: Path<'a>,
    pub imports: Vec<Import<'a>>,
    pub import_span: [usize; 2],
    pub classes: Vec<Class<'a>>,
}

#[derive(Debug, Default, PartialEq)]
pub struct Import<'a> {
    pub path: Path<'a>,
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
    pub type_name: Path<'a>,
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

pub fn parse_file(source: &str) -> Result<Parse, Error> {
    let parser = &mut Parser::new(source);
    parser.skip_whitespace();
    parser.expect_keyword(b"package")?;

    let package = parse_path(parser).ok_or(failure::err_msg("Expected package name"))?;
    parser.expect(b";")?;

    let import_start_point = parser.cursor();
    let imports = parse_imports(parser)?;
    let import_end_point = parser.cursor();

    let mut classes = vec![];
    while !parser.finished() {
        classes.push(parse_class(parser)?);
    }

    Ok(Parse {
        source,
        package,
        imports,
        import_span: [import_start_point, import_end_point],
        classes,
    })
}

fn parse_path<'a>(parser: &mut Parser<'a>) -> Option<Path<'a>> {
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
        Some(Path(path))
    }
}

fn parse_imports<'a>(parser: &mut Parser<'a>) -> Result<Vec<Import<'a>>, Error> {
    let mut imports = vec![];

    while parser.skip_keyword(b"import") {
        let is_static = parser.skip_keyword(b"static");
        let path = parse_path(parser).ok_or(failure::err_msg("Expected import path"))?;
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

fn parse_class<'a>(parser: &mut Parser<'a>) -> Result<Class<'a>, Error> {
    skip_visibility(parser);

    while parser.skip(b"static") || parser.skip(b"final") || parser.skip(b"abstract") {
    }

    let is_enum = parser.skip_keyword(b"enum");
    let is_item = is_enum || parser.skip_keyword(b"class") || parser.skip_keyword(b"interface");

    if !is_item {
        bail!("Expected class, interface, or enum");
    }

    let name = parser.expect_ident()?;

    let mut type_params = vec![];
    if parser.skip(b"<") {
        type_params = parse_comma_separated(parser, Parser::skip_ident);
        parser.expect(b">")?;
    }

    let mut subtypes = vec![];
    if parser.skip_keyword(b"extends") {
        subtypes.push(parse_type(parser)?.ok_or(failure::err_msg(
            "Expected superclass after `extends` keyword",
        ))?);
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

            while parser.skip(b"static") || parser.skip(b"final") {
            }

            let possible_constructor_name = parser.check_ident();
            let item_type = parse_type(parser)?.ok_or(failure::err_msg("Expected a type name"))?;
            if parser.skip(b"(") {
                let args = try_parse_comma_separated(parser, parse_arg)?;
                parser.expect(b")")?;
                let throws = if parser.skip_keyword(b"throws") { try_parse_comma_separated(parser, parse_type)? } else { vec![] };
                parser.skip_around(b'{', b'}')?;

                let constructor = Method {
                    annotations: vec![],
                    type_params: vec![],
                    return_type: item_type,
                    name: possible_constructor_name
                        .ok_or(failure::err_msg("Expected constructor name"))?,
                    args,
                    throws,
                };
                methods.push(constructor);
            } else {
                let name = parser.expect_ident()?;
                if parser.skip(b"(") {
                    let args = try_parse_comma_separated(parser, parse_arg)?;
                    parser.expect(b")")?;
                    let throws = if parser.skip_keyword(b"throws") { try_parse_comma_separated(parser, parse_type)? } else { vec![] };
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

fn parse_type<'a>(parser: &mut Parser<'a>) -> Result<Option<Type<'a>>, Error> {
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

fn parse_arg<'a>(parser: &mut Parser<'a>) -> Result<Option<Arg<'a>>, Error> {
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

fn try_parse_comma_separated<'a, T, F>(parser: &mut Parser<'a>, f: F) -> Result<Vec<T>, Error>
where
    F: Fn(&mut Parser<'a>) -> Result<Option<T>, Error>,
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
    testcase!(path_of_one, "one", parse_path, Some(Path(vec![b"one"])), "");
    testcase!(
        path_of_two,
        "one.Two",
        parse_path,
        Some(Path(vec![b"one", b"Two"])),
        ""
    );
    testcase!(
        path_trims,
        "one.Two  ;",
        parse_path,
        Some(Path(vec![b"one", b"Two"])),
        ";"
    );

    fn mkimport(path: &[&'static [u8]], is_static: bool, star: bool) -> Import<'static> {
        Import {
            path: Path(path.to_vec()),
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
            type_name: Path(path.to_owned()),
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
            fields: vec![Field { field_type: mktype(&[b"int"], &[]), name: b"size", ..Field::default() }],
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
            fields: vec![Field { field_type: mktype(&[b"int"], &[]), name: b"size", ..Field::default() }],
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
            fields: vec![Field { field_type: mktype(&[b"int"], &[]), name: b"size", ..Field::default() }],
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
            assert_eq!(parse_file(src).unwrap().package, Path(package.to_vec()));
        }
    }
}
