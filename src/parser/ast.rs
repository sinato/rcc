use crate::lexer::token::Token;
use inkwell::builder::Builder;
use inkwell::context::Context;
use inkwell::module::Module;
use inkwell::values::{IntValue, PointerValue};
use std::collections::HashMap;
use std::{fmt, path};


pub struct Emitter {
    context: Context,
    builder: Builder,
    module: Module,
    variables: HashMap<String, PointerValue>
}
impl Emitter {
    pub fn new() -> Emitter {
        let context = Context::create();
        let module = context.create_module("my_module");
        let builder = context.create_builder();
        let variables = HashMap::new();

        Emitter {
            context,
            builder, 
            module,
            variables
        }
    }
    pub fn print_to_file(&self, filename: &str) {
        let _ = self.module.print_to_file(path::Path::new(filename));
    }
    pub fn emit(&mut self, statements: Statements) {
        // generate function prototype
        let function = self.module.add_function("main", self.context.i32_type().fn_type(&[], false), None);
        let basic_block = self.context.append_basic_block(&function, "entry");
        self.builder.position_at_end(&basic_block);
        self.emit_statement(statements)
    }
    fn emit_statement(&mut self, statements: Statements) {
        let asts = statements.asts.clone();
        let ret = asts.into_iter().map(|ast| self.emit_ast(ast)).last();
        match ret {
            Some(ret) => self.builder.build_return(Some(&ret)),
            None => panic!("Emit Error: Expect at least an ast."),
        };
    }
    fn emit_ast(&mut self, ast: Ast) -> IntValue {
        let ast_node = ast.ast;
        self.emit_ast_node(ast_node)
    }
    fn emit_ast_node(&mut self, ast_node: AstNode) -> IntValue {
        match ast_node {
            AstNode::Exp(ast) => self.emit_ast_exp(ast),
            AstNode::Fin(ast) => self.emit_ast_fin(ast),
            AstNode::Bind(ast) => self.emit_ast_bind(ast),
        }
    }
    fn emit_ast_exp(&mut self, ast_binary_exp: AstBinaryExp) -> IntValue {
        let lhs_num = self.emit_ast_node(*ast_binary_exp.lhs.clone());
        let rhs_num = self.emit_ast_node(*ast_binary_exp.rhs.clone());
        let op = ast_binary_exp.get_op_string();
        match op.as_ref() {
            "+" => self.builder.build_int_add(lhs_num, rhs_num, "sum"),
            "*" => self.builder.build_int_mul(lhs_num, rhs_num, "mul"),
            _ => panic!("Emit Error: Not implemented operator"),
        }
    }
    fn emit_ast_fin(&self, ast_fin: AstFin) -> IntValue {
        let ast_fin_enum = ast_fin.fin;
        match ast_fin_enum {
            AstFinEnum::Num(ast) => self.emit_ast_num(ast),
            AstFinEnum::Ide(ast) => self.emit_ast_ide(ast),
        }
    }
    fn emit_ast_bind(&mut self, ast_binding: AstBinding) -> IntValue {
        let identifier = ast_binding.ide.get_identifier();
        let pointer = self.builder.build_alloca(self.context.i32_type(), &identifier);
        let val = self.emit_ast_node(*ast_binding.val);

        self.builder.build_store(pointer, val);
        self.variables.insert(identifier, pointer);
        val
    }
    fn emit_ast_num(&self, ast_num: AstNum) -> IntValue {
        self.context.i32_type().const_int(ast_num.num.get_num(), false)
    }
    fn emit_ast_ide(&self, ast_ide: AstIde) -> IntValue {
        let allocation = self.variables.get(&ast_ide.get_identifier());
        match allocation {
            Some(pointer) => self.builder.build_load(*pointer, &ast_ide.ide.get_ide()).into_int_value(),
            None => panic!("Emit Error: Undeclared variable."),
        }
    }
}


#[derive(Debug, PartialEq, Clone)]
pub struct Statements {
    pub asts: Vec<Ast>,
    variables: HashMap<String, PointerValue>,
}
impl Statements {
    pub fn new(asts: Vec<Ast>) -> Statements {
        let variables = HashMap::new();
        Statements { asts, variables }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct AstBinding {
    pub ide: AstIde,
    pub val: Box<AstNode>,
}

#[derive(Debug, PartialEq, Clone)]
pub struct AstBinaryExp {
    pub op: AstOp,
    pub lhs: Box<AstNode>,
    pub rhs: Box<AstNode>,
}
impl AstBinaryExp {
    fn get_op_string(&self) -> String {
        self.op.get_op()
    }
}
impl fmt::Display for AstBinaryExp {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(
            f,
            "OP: {:?}\nLHS: {:?}\nRHS: {:?}\n",
            self.op, *self.lhs, *self.rhs
        )
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct AstFin {
    pub fin: AstFinEnum,
}
impl AstFin {
    pub fn new_from_num_token(num: Token) -> AstFin {
        AstFin { fin: AstFinEnum::Num(AstNum { num })}
    }
    pub fn new_from_ide_token(ide: Token) -> AstFin {
        AstFin { fin: AstFinEnum::Ide(AstIde { ide })}
    }
}
#[derive(Debug, PartialEq, Clone)]
pub enum AstFinEnum {
    Num(AstNum),
    Ide(AstIde),
}

#[derive(Debug, PartialEq, Clone)]
pub struct AstNum {
    pub num: Token,
}

#[derive(Debug, PartialEq, Clone)]
pub struct AstOp {
    pub op: Token,
}
impl AstOp {
    fn get_op(&self) -> String {
        match self.op.clone() {
            Token::Op(s) => s,
            _ => panic!("expect operator"),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct AstIde {
    pub ide: Token,
}
impl AstIde {
    fn get_identifier(&self) -> String {
        match self.ide.clone() {
            Token::Ide(s) => s,
            _ => panic!("expect identifier"),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum AstNode {
    Exp(AstBinaryExp),
    Fin(AstFin),
    Bind(AstBinding),
}
impl fmt::Display for AstNode {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            AstNode::Exp(ast_binary_exp) => write!(f, "EXP: {}\n", ast_binary_exp),
            AstNode::Fin(ast) => write!(f, "FIN: {:?}\n", ast),
            AstNode::Bind(ast_binding) => write!(f, "BIND: {:?}\n", ast_binding),
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Ast {
    pub ast: AstNode,
}
