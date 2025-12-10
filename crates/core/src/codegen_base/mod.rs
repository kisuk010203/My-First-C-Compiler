use crate::{
    grammar::{BlockStmt, Expression, Program, Statement, UnaryOp},
    ir_base::{self, IRFuncDef, IRProgram, Instruction, Operand},
    r,
};

#[derive(Default)]
pub struct CodeGenerator {
    current_function: Vec<Instruction>,
    #[allow(dead_code)]
    label_counter: usize,
}

impl CodeGenerator {
    pub fn new() -> Self {
        Self::default()
    }

    #[allow(dead_code)]
    fn generate_label(&mut self, prefix: &str) -> String {
        let label = format!(".L{}_{}", prefix, self.label_counter);
        self.label_counter += 1;
        label
    }

    fn emit(&mut self, inst: Instruction) {
        self.current_function.push(inst);
    }

    pub fn generate<'a>(mut self, program: &'a Program<'a>) -> IRProgram<'a> {
        program
            .functions
            .iter()
            .map(|f| self.generate_function(f))
            .collect::<IRProgram<'a>>()
    }

    fn generate_function<'a>(
        &mut self,
        func: &'a crate::grammar::FuncDef<'a>,
    ) -> ir_base::IRFuncDef<'a> {
        // function prologue
        self.emit(Instruction::Push(Operand::Register(r!("rbp"))));
        self.emit(Instruction::Mov {
            src: Operand::Register(r!("rsp")),
            dst: Operand::Register(r!("rbp")),
        });

        // generate body
        self.generate_block_statement(&func.body);

        // function epilogue
        self.emit(Instruction::Pop(Operand::Register(r!("rbp"))));
        self.emit(Instruction::Ret);

        IRFuncDef::new_global(func.name.clone(), self.current_function.drain(..).collect())
    }

    fn generate_statement<'a>(&mut self, stmt: &Statement<'a>) {
        match stmt {
            Statement::Return(ret) => {
                self.generate_expression(&ret.expr);
                self.emit(Instruction::Pop(Operand::Register(r!("rbp"))));
                self.emit(Instruction::Ret);
            }
            Statement::Block(block) => {
                for s in &block.statements {
                    self.generate_statement(s);
                }
            }
            Statement::Null(_) => {}
            Statement::Break(_) => todo!(),
            Statement::Continue(_) => todo!(),
            Statement::Declaration(_) => todo!(),
            Statement::DoWhile(_) => todo!(),
            Statement::Expr(_) => todo!(),
            Statement::For(_) => todo!(),
            Statement::If(_) => todo!(),
            Statement::While(_) => todo!(),
        }
    }

    fn generate_block_statement<'a>(&mut self, block: &BlockStmt<'a>) {
        for s in &block.statements {
            self.generate_statement(s);
        }
    }

    fn generate_expression<'a>(&mut self, expr: &Expression<'a>) {
        match expr {
            Expression::Constant(val) => self.emit(Instruction::Mov {
                src: Operand::Immediate(*val),
                dst: Operand::Register(r!("rax")),
            }),
            Expression::Variable(_) => todo!(),
            Expression::Grouped(_) => todo!(),
            Expression::Binary {
                op: _op,
                lhs: _lhs,
                rhs: _rhs,
            } => todo!(),
            Expression::Unary { op, expr } => {
                self.generate_expression(expr);
                match op {
                    UnaryOp::Negate => {
                        self.emit(Instruction::Neg {
                            dst: Operand::Register(r!("rax")),
                        });
                    }
                    UnaryOp::Not => {
                        todo!()
                    }
                }
            }
            Expression::Assignment {
                op: _op,
                lhs: _lhs,
                rhs: _rhs,
            } => todo!(),
            Expression::FunctionCall {
                callee: _callee,
                args: _args,
            } => todo!(),
        }
    }
}
