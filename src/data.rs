use std::{
    borrow::Cow,
    fmt::{self, Display, Formatter},
};

#[derive(Debug, Default, Clone, PartialEq)]
pub struct Ident<S>(pub S);

#[derive(Debug, Default, Clone, PartialEq)]
pub struct Scoped<S>(pub Vec<Ident<S>>);

#[derive(Debug, Default, Clone, PartialEq)]
pub struct SymbolSoup<S> {
    pub idents: Vec<Ident<S>>,
}

#[derive(Debug, Default, Clone, PartialEq)]
pub struct Import<S> {
    pub path: Scoped<S>,
    pub is_static: bool,
    pub star: bool,
}

#[derive(Debug, Default, Clone, PartialEq)]
pub struct Type<S> {
    pub type_name: Scoped<S>,
    pub type_params: Vec<Type<S>>,
    pub is_array: bool,
}

#[derive(Debug, Default, Clone, PartialEq)]
pub struct Field<S> {
    pub annotations: Vec<Type<S>>,
    pub name: Ident<S>,
    pub field_type: Type<S>,
    pub value: SymbolSoup<S>,
}

#[derive(Debug, Default, Clone, PartialEq)]
pub struct Arg<S> {
    pub annotations: Vec<Type<S>>,
    pub name: Ident<S>,
    pub arg_type: Type<S>,
}

#[derive(Debug, Default, Clone, PartialEq)]
pub struct Method<S> {
    pub annotations: Vec<Type<S>>,
    pub type_params: Vec<Ident<S>>,
    pub return_type: Type<S>,
    pub name: Ident<S>,
    pub args: Vec<Arg<S>>,
    pub throws: Vec<Type<S>>,
}

#[derive(Debug, Default, Clone, PartialEq)]
pub struct Class<S> {
    pub annotations: Vec<Type<S>>,
    pub name: Ident<S>,
    pub fields: Vec<Field<S>>,
    pub methods: Vec<Method<S>>,
    pub variants: Vec<Ident<S>>,
    pub type_params: Vec<Ident<S>>,
    pub subtypes: Vec<Type<S>>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct ParseTree<'a, S> {
    pub source: &'a str,
    pub package: Scoped<S>,
    pub imports: Vec<Import<S>>,
    pub import_span: [usize; 2],
    pub classes: Vec<Class<S>>,
}

pub type Parse<'a> = ParseTree<'a, &'a [u8]>;
pub type CowParse<'a> = ParseTree<'a, Cow<'a, [u8]>>;

impl<'a, 'b> ParseTree<'b, &'a [u8]> {
    pub fn into_cow(self) -> ParseTree<'b, Cow<'a, [u8]>> {
        self.into()
    }
}

impl<S> Display for Ident<S>
where
    S: AsRef<[u8]>,
{
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        std::str::from_utf8(self.0.as_ref())
            .unwrap_or("<binary>")
            .fmt(f)
    }
}

impl<S> Display for Scoped<S>
where
    Ident<S>: Display,
{
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        self.0
            .iter()
            .map(Ident::to_string)
            .collect::<Vec<_>>()
            .join(".")
            .fmt(f)
    }
}

impl<S> Display for Import<S>
where
    Ident<S>: Display,
{
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        let static_label = if self.is_static { "static " } else { "" };
        let star_label = if self.star { ".*" } else { "" };
        writeln!(f, "import {}{}{};", static_label, self.path, star_label)
    }
}

impl<S> Display for Type<S>
where
    Ident<S>: Display
{
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        self.type_name.fmt(f)?;
        if !self.type_params.is_empty() {
            write!(f, "<")?;
            self.type_params.iter().map(Type::to_string).collect::<Vec<_>>().join(", ").fmt(f)?;
            write!(f, ">")?;
        }
        if self.is_array {
            write!(f, "[]")?;
        }
        Ok(())
    }
}

impl<S> Display for Field<S>
where
    Ident<S>: Display
{
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        for annotation in &self.annotations {
            writeln!(f, "@{}", annotation)?;
        }
        writeln!(f, "{} {};", self.field_type, self.name)
    }
}

impl<S> Display for Arg<S>
where
    Ident<S>: Display
{
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        for annotation in &self.annotations {
            write!(f, "@{} ", annotation)?;
        }
        write!(f, "{} ", self.arg_type)?;
        write!(f, "{}", self.name)
    }
}

impl<S> Display for Method<S>
where
    Ident<S>: Display
{
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        for annotation in &self.annotations {
            writeln!(f, "@{}", annotation)?;
        }

        if !self.type_params.is_empty() {
            write!(f, "<")?;
            self.type_params.iter().map(Ident::to_string).collect::<Vec<_>>().join(", ").fmt(f)?;
            write!(f, "> ")?;
        }

        write!(f, "{} {}(", self.return_type, self.name)?;
        self.args.iter().map(Arg::to_string).collect::<Vec<_>>().join(", ").fmt(f)?;
        write!(f, ")")?;

        if !self.throws.is_empty() {
            write!(f, " throws ")?;
            self.throws.iter().map(Type::to_string).collect::<Vec<_>>().join(", ").fmt(f)?;
        }

        writeln!(f, " {{}}")
    }
}

impl<S> Display for Class<S>
where
    Ident<S>: Display
{
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        for annotation in &self.annotations {
            writeln!(f, "@{}", annotation)?;
        }

        write!(f, "class {}", self.name)?;
        if !self.type_params.is_empty() {
            write!(f, "<")?;
            self.type_params.iter().map(Ident::to_string).collect::<Vec<_>>().join(", ").fmt(f)?;
            write!(f, ">")?;
        }

        if !self.subtypes.is_empty() {
            write!(f, " implements ")?;
            self.subtypes.iter().map(Type::to_string).collect::<Vec<_>>().join(", ").fmt(f)?;
        }

        writeln!(f, "{{")?;
        if !self.variants.is_empty() {
            self.variants.iter().map(Ident::to_string).collect::<Vec<_>>().join(",\n").fmt(f)?;
            writeln!(f, ";")?;
        }

        for field in &self.fields {
            field.fmt(f)?;
        }

        for method in &self.methods {
            method.fmt(f)?;
        }

        writeln!(f, "}}")?;

        Ok(())
    }
}

impl<'a, S> Display for ParseTree<'a, S>
where
    Ident<S>: Display
{
    fn fmt(&self, f: &mut Formatter) -> fmt::Result {
        writeln!(f, "package {};\n", self.package)?;
        for import in &self.imports {
            import.fmt(f)?;
        }
        for class in &self.classes {
            class.fmt(f)?;
        }
        Ok(())
    }
}

impl<'a> From<Ident<&'a [u8]>> for Ident<Cow<'a, [u8]>> {
    fn from(borrowed: Ident<&'a [u8]>) -> Self {
        Ident(Cow::Borrowed(borrowed.0))
    }
}

impl<'a> From<Scoped<&'a [u8]>> for Scoped<Cow<'a, [u8]>> {
    fn from(borrowed: Scoped<&'a [u8]>) -> Self {
        Scoped(borrowed.0.into_iter().map(Into::into).collect::<Vec<_>>())
    }
}

impl<'a> From<SymbolSoup<&'a [u8]>> for SymbolSoup<Cow<'a, [u8]>> {
    fn from(borrowed: SymbolSoup<&'a [u8]>) -> Self {
        SymbolSoup {
            idents: borrowed.idents.into_iter().map(Into::into).collect::<Vec<_>>(),
        }
    }
}

impl<'a> From<Import<&'a [u8]>> for Import<Cow<'a, [u8]>> {
    fn from(borrowed: Import<&'a [u8]>) -> Self {
        Import {
            path: borrowed.path.into(),
            is_static: borrowed.is_static,
            star: borrowed.star,
        }
    }
}

impl<'a> From<Type<&'a [u8]>> for Type<Cow<'a, [u8]>> {
    fn from(borrowed: Type<&'a [u8]>) -> Self {
        Type {
            type_name: borrowed.type_name.into(),
            type_params: borrowed.type_params.into_iter().map(Into::into).collect::<Vec<_>>(),
            is_array: borrowed.is_array,
        }
    }
}

impl<'a> From<Field<&'a [u8]>> for Field<Cow<'a, [u8]>> {
    fn from(borrowed: Field<&'a [u8]>) -> Self {
        Field {
            annotations: borrowed.annotations.into_iter().map(Into::into).collect::<Vec<_>>(),
            name: borrowed.name.into(),
            field_type: borrowed.field_type.into(),
            value: borrowed.value.into(),
        }
    }
}

impl<'a> From<Arg<&'a [u8]>> for Arg<Cow<'a, [u8]>> {
    fn from(borrowed: Arg<&'a [u8]>) -> Self {
        Arg {
           annotations: borrowed.annotations.into_iter().map(Into::into).collect::<Vec<_>>(),
           name: borrowed.name.into(),
           arg_type: borrowed.arg_type.into(),
        }
    }
}

impl<'a> From<Method<&'a [u8]>> for Method<Cow<'a, [u8]>> {
    fn from(borrowed: Method<&'a [u8]>) -> Self {
        Method {
            annotations: borrowed.annotations.into_iter().map(Into::into).collect::<Vec<_>>(),
            type_params: borrowed.type_params.into_iter().map(Into::into).collect::<Vec<_>>(),
            return_type: borrowed.return_type.into(),
            name: borrowed.name.into(),
            args: borrowed.args.into_iter().map(Into::into).collect::<Vec<_>>(),
            throws: borrowed.throws.into_iter().map(Into::into).collect::<Vec<_>>(),
        }
    }
}

impl<'a> From<Class<&'a [u8]>> for Class<Cow<'a, [u8]>> {
    fn from(borrowed: Class<&'a [u8]>) -> Self {
        Class {
            annotations: borrowed.annotations.into_iter().map(Into::into).collect::<Vec<_>>(),
            name: borrowed.name.into(),
            fields: borrowed.fields.into_iter().map(Into::into).collect::<Vec<_>>(),
            methods: borrowed.methods.into_iter().map(Into::into).collect::<Vec<_>>(),
            variants: borrowed.variants.into_iter().map(Into::into).collect::<Vec<_>>(),
            type_params: borrowed.type_params.into_iter().map(Into::into).collect::<Vec<_>>(),
            subtypes: borrowed.subtypes.into_iter().map(Into::into).collect::<Vec<_>>(),
        }
    }
}

impl<'a, 'b> From<ParseTree<'b, &'a [u8]>> for ParseTree<'b, Cow<'a, [u8]>> {
    fn from(borrowed: ParseTree<'b, &'a [u8]>) -> Self {
        ParseTree {
            source: borrowed.source,
            package: borrowed.package.into(),
            imports: borrowed.imports.into_iter().map(Into::into).collect::<Vec<_>>(),
            import_span: borrowed.import_span,
            classes: borrowed.classes.into_iter().map(Into::into).collect::<Vec<_>>(),
        }
    }
}
