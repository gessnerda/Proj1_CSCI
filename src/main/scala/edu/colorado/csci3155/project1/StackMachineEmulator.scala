package edu.colorado.csci3155.project1

import scala.annotation.tailrec



sealed trait StackMachineInstruction
/*-- Complete the byte code instructions as specified in the documentation --*/
case class LoadEnv(s: String) extends StackMachineInstruction
case class  StoreEnv(s: String) extends StackMachineInstruction
case object PopEnv extends StackMachineInstruction

case class PushNumI(f: Double) extends StackMachineInstruction
case class PushBoolI(b: Boolean) extends StackMachineInstruction
case object AddI extends StackMachineInstruction
case object SubI extends StackMachineInstruction
case object MultI extends StackMachineInstruction
case object DivI extends StackMachineInstruction
case object ExpI extends StackMachineInstruction
case object LogI extends StackMachineInstruction
case object SinI extends StackMachineInstruction
case object CosI extends StackMachineInstruction
case object GeqI extends StackMachineInstruction
case object EqI extends StackMachineInstruction 
case object NotI extends StackMachineInstruction
case object PopI extends StackMachineInstruction

case class CSkipI(numToSkip: Int) extends StackMachineInstruction
case class SkipI(numToSkip: Int) extends StackMachineInstruction

object StackMachineEmulator {

    /*-- An environment stack is a list of tuples containing strings and values --*/
    type RuntimeStack = List[(String, Value)]
    /*-- An operand stack is a list of values --*/
    type OpStack = List[Value]

    /* Function emulateSingleInstruction
        Given a list of values to represent a operand stack
              a list of tuples (string, value) to represent runtime stack
        and   a single instruction of type StackMachineInstruction
        Return a tuple that contains the
              modified stack that results when the instruction is executed.
              modified runtime that results when the instruction is executed.

        Make sure you handle the error cases: eg., stack size must be appropriate for the instruction
        being executed. Division by zero, log of a non negative number
        Throw an exception or assertion violation when error happens.
     */
    def emulateSingleInstruction(stack: OpStack,
                                 env: RuntimeStack,
                                 ins: StackMachineInstruction): (OpStack, RuntimeStack) = {
        ins match {
            /*TODO:  Your code here must handle each instruction type and 
                     execute the appropriate instructions to modify the 
                     runtime/operand stacks as specified */
            case PushNumI(f) => (Num(f) :: stack, env)
            case PushBoolI(b) => (Bool(b) :: stack, env)

            case AddI => {
                stack match {
                    case Num(n1) :: Num(n2) :: t => {
                        val np = n1 + n2
                        val vp = Num(np)
                        val newStack = vp :: t
                        (newStack, env)
                    }
                    case _ => throw new RuntimeException("Error")
                }
            }
            case MultI => {
                stack match {
                    case Num(n1) :: Num(n2) :: t => {
                        val np = n1 * n2
                        val vp = Num(np)
                        val newStack = vp :: t
                        (newStack, env)
                    }
                    case _ => throw new RuntimeException("Error")
                }
            }
            case SubI => {
                stack match {
                    case Num(n1) :: Num(n2) :: t => {
                        val np = n2 - n1
                        val vp = Num(np)
                        val newStack = vp :: t
                        (newStack, env)
                    }
                    case _ => throw new RuntimeException("Error")
                }
            }
            case DivI => {
                stack match {
                    case Num(n1) :: Num(n2) :: t => {
                        if( n1 == 0){
                            throw new RuntimeException("division by 0")
                        }
                        val np = n2 / n1
                        val vp = Num(np)
                        val newStack = vp :: t
                        (newStack, env)
                    }
                    case _ => throw new RuntimeException("Error")
                }
            }
            case GeqI => {
                stack match {
                    case Num(n1) :: Num(n2) :: t => {
                        val np = (n2 >= n1)
                        val vp = Bool(np)
                        val newStack = vp :: t
                        (newStack, env)
                    }
                    case _ => throw new RuntimeException("Error")
                }
            }
            case EqI => {
                stack match {
                    case Num(n1) :: Num(n2) :: t => {
                        val np = (n2 == n1)
                        val vp = Bool(np)
                        val newStack = vp :: t
                        (newStack, env)
                    }
                    case Bool(n1) :: Bool(n2) :: t => {
                        val np = (n2 == n1)
                        val vp = Bool(np)
                        val newStack = vp :: t
                        (newStack, env)
                    }
                    case _ => throw new RuntimeException("Error")
                }
            }
            case NotI => {
                stack match {
                    case Bool(n) :: t => {
                        val newBool = !n
                        val newStack = Bool(newBool) :: t
                        (newStack, env)
                    }
                    case _ => throw new RuntimeException("Error")
                }
            }
            case SinI => {
                stack match {
                    case Num(n) :: t => {
                        val n1 = scala.math.sin(n)
                        val nn = Num(n1)
                        val newStack = nn :: t 
                        (newStack,env)
                    }
                    case _ => throw new RuntimeException("Error")
                }
            }
            case CosI => {
                stack match {
                    case Num(n) :: t => {
                        val n1 = scala.math.cos(n)
                        val nn = Num(n1)
                        val newStack = nn :: t 
                        (newStack,env)
                    }
                    case _ => throw new RuntimeException("Error")
                }
            }

            case PopI => { 
                if (stack.isEmpty) {
                    throw new RuntimeException("Stack is empty")
                }
                else{
                    (stack.tail,env)
                }
            }

            case PopEnv => {
                if (env.isEmpty){
                    throw new RuntimeException("Stack is empty")
                }
                else {
                    (stack,env.tail)
                }
            }

            case ExpI => {
                stack match {
                    case Num(n) :: t => {
                        val n1 = scala.math.exp(n)
                        val nn = Num(n1)
                        val newStack = nn :: t 
                        (newStack,env)
                    }
                    case _ => throw new RuntimeException("Error")
                }
            }

            case LogI => {
                stack match {
                    case Num(n) :: t => {
                        if (n > 0) {
                            val n1 = scala.math.log(n)
                            val nn = Num(n1)
                            val newStack = nn :: t 
                            (newStack,env)
                        }
                        else {
                            throw new RuntimeException("Not positive")
                        }
                    }
                    case _ => throw new RuntimeException("Error")
                }
            }
            case LoadEnv(s) => {
                stack match {
                    case Num(n) :: t => {
                        val pair = (s,Num(n))
                        println(pair)
                        val newStack = t
                        (newStack, pair :: env)
                    }
                    case Bool(n) :: t => {
                        val v = n
                        val pair = (s,Bool(v))
                        val newStack = t
                        (newStack, pair :: env)
                    }
                    case _ => throw new RuntimeException("Error")
                }
            }
            case StoreEnv(s) => {
                val foundS = env.filter{case (ss,v) => ss == s}
                if (foundS.isEmpty){
                    throw new RuntimeException("Not found")
                }
                else{
                    (foundS.head._2 :: stack, env)
                }
            }

            
            
            case _ => throw new RuntimeException(s"Unknown instruction type: $ins ")
        }
        
    }

    /* Function emulateStackMachine
       Execute the list of instructions provided as inputs using the
       emulateSingleInstruction function.
       Return the final runtimeStack and the top element of the opStack
     */
    @tailrec
    def emulateStackMachine(instructionList: List[StackMachineInstruction], 
                            opStack: OpStack=Nil, 
                            runtimeStack: RuntimeStack=Nil): (Value, RuntimeStack) =
        {
            /*-- Are we out of instructions to execute --*/
            if (instructionList.isEmpty){
                /*-- output top elt. of operand stack and the runtime stack --*/
                (opStack.head, runtimeStack)
            } else {
                /*- What is the instruction on top -*/
                val ins = instructionList.head
                ins match {
                    /*-- Conditional skip instruction --*/
                    case CSkipI(n) => {
                        /* get the top element in operand stack */
                        val topElt = opStack.head 
                        val restOpStack = opStack.tail 
                        val b = topElt.getBooleanValue /* the top element better be a boolean */
                        if (!b) {
                            /*-- drop the next n instructions --*/
                            val restOfInstructions = instructionList.drop(n+1)
                            emulateStackMachine(restOfInstructions, restOpStack, runtimeStack)
                        } else {
                            /*-- else just drop this instruction --*/
                            emulateStackMachine(instructionList.tail, restOpStack, runtimeStack)
                        }
                    }
                    case SkipI(n) => {
                        /* -- drop this instruction and next n -- continue --*/
                        emulateStackMachine(instructionList.drop(n+1), opStack, runtimeStack)
                    }

                    case _ => {
                        /*- Otherwise, just call emulateSingleInstruction -*/
                        val (newOpStack: OpStack, newRuntime:RuntimeStack) = emulateSingleInstruction(opStack, runtimeStack, ins)
                        emulateStackMachine(instructionList.tail, newOpStack, newRuntime)
                    }
                }
            }
        }
}