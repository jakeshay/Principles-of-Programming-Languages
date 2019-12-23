package edu.colorado.csci3155.project1

object StackMachineCompiler {

    /* Function compileToStackMachineCode
        Given expression e as input, return a corresponding list of stack machine instructions.
        The type of stackmachine instructions are in the file StackMachineEmulator.scala in this same directory
        The type of Expr is in the file Expr.scala in this directory.
     */
    def compileToStackMachineCode(e: Expr): List[StackMachineInstruction] = {
        e match {
            case Const(f) => {
                List(PushI(f))
            }
            case Plus(e1, e2) => {
                compileToStackMachineCode(e1)++compileToStackMachineCode(e2)++List(AddI)
            }
            case Minus(e1, e2) => {
                compileToStackMachineCode(e1)++compileToStackMachineCode(e2)++List(SubI)
            }
            case Mult(e1, e2) => {
                compileToStackMachineCode(e1)++compileToStackMachineCode(e2)++List(MultI)
            }
            case Div(e1, e2) => {
                compileToStackMachineCode(e1)++compileToStackMachineCode(e2)++List(DivI)
            }
            case Exp(e) => {
                compileToStackMachineCode(e)++List(ExpI)
            }
            case Log(e) => {
                compileToStackMachineCode(e)++List(LogI)
            }
            case Sine(e) => {
                compileToStackMachineCode(e)++List(SinI)
            }
            case Cosine(e) => {
                compileToStackMachineCode(e)++List(CosI)
            }

        }

        }
}
