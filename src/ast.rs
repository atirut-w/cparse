pub struct TranslationUnit {
    pub function_definition: FunctionDefinition,
}

pub struct FunctionDefinition {
    pub name: String,
    pub body: Statement,
}

pub enum Statement {
    Return(Expression),
}

pub enum Expression {
    IntConstant(i64),
}
