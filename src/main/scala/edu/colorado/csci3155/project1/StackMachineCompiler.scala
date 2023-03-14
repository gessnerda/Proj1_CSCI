package edu.colorado.csci3155.project1

import StackMachineEmulator._

object StackMachineCompiler {

    def compileToStackMachineCode(e: Expr): List[StackMachineInstruction] = {
        e match {
            case Const(f) => List(PushNumI(f))

            case Plus(e1, e2) => compileToStackMachineCode(e1) ++ compileToStackMachineCode(e2) ++ List(AddI)

            case Minus(e1, e2) => compileToStackMachineCode(e1) ++ compileToStackMachineCode(e2) ++ List(SubI)

            case Mult(e1, e2) => compileToStackMachineCode(e1) ++ compileToStackMachineCode(e2) ++ List(MultI)

            case Div(e1, e2) => compileToStackMachineCode(e1) ++ compileToStackMachineCode(e2) ++ List(DivI)

            case Exp(e1) => compileToStackMachineCode(e1) ++ List(ExpI)

            case Log(e1) => compileToStackMachineCode(e1) ++ List(LogI)

            case Sine(e1) => compileToStackMachineCode(e1) ++ List(SinI)

            case Cosine(e1) => compileToStackMachineCode(e1) ++ List(CosI)

            case Geq(e1, e2) => compileToStackMachineCode(e1) ++ compileToStackMachineCode(e2) ++ List(GeqI)

            case Eq(e1, e2) => compileToStackMachineCode(e1) ++ compileToStackMachineCode(e2) ++ List(EqI)

            case And(e1, e2) => compileToStackMachineCode(IfThenElse(e1, e2, Const(0)))

            case Or(e1, e2) => compileToStackMachineCode(IfThenElse(e1, Const(1), e2))

            case Not(e1) => compileToStackMachineCode(e1) ++ List(NotI)

            case Let(s, e1, e2) =>
                compileToStackMachineCode(e1) ++ List(PushEnv) ++ List(StoreEnv(s)) ++
                compileToStackMachineCode(e2) ++ List(PopEnv)


            case IfThenElse(e1, e2, e3) =>
                val e1Instr = compileToStackMachineCode(e1)
                val e2Instr = compileToStackMachineCode(e2)
                val e3Instr = compileToStackMachineCode(e3)
                e1Instr ++ List(CSkipI(e2Instr.length + 1)) ++ e2Instr ++ List(SkipI(e3Instr.length)) ++ e3Instr

            case Ident(s) => List(LoadEnv(s))
            

            case _ => throw new IllegalArgumentException("Invalid expression")
        }
    }
}
