package anu.rsise.typeChecker;

import java.util.TreeMap;

import anu.rsise.certParser.BasicLevel;
import anu.rsise.certParser.BytecodeMethod;
import anu.rsise.certParser.CertContainer;
import anu.rsise.certParser.ExtendedLevel;
import anu.rsise.certParser.LevelPool;
import anu.rsise.certParser.LevelRelationship;
import anu.rsise.certParser.MethodCert;
import anu.rsise.certParser.MethodPolicy;
import anu.rsise.certParser.Typing;
import anu.rsise.dexParser.container.ClassContainer;
import anu.rsise.dexParser.container.ClassContainer.ClassItem;
import anu.rsise.dexParser.container.DexContainer;
import anu.rsise.dexParser.encoding.CodeInterpreter;
import anu.rsise.dexParser.encoding.CodeItem;

public class TypeChecker {
    private final String failedConstraint = "Failed constraint checking at ";
    private final String failedRT = "Failed registers type checking at ";
    private final String failedFlag = "Failed flag checking at ";
    private final String failedSE = "Security environment is not adequate for label ";
    private CertContainer _cert;
    private DexContainer _file;

    public TypeChecker(CertContainer cert, DexContainer file) {
        _cert = cert;
        _file = file;
    }

    private String error_message(String prefix, int label, String comment) {
        StringBuilder sb = new StringBuilder();
        sb.append(prefix);
        sb.append(label);
        sb.append(":");
        sb.append(comment);
        return sb.toString();
    }

    private Result traverseBytecode_perLevel(CodeItem bm, MethodCert m_cert, LevelRelationship lvl_rel, int object_level) {
        BytecodeMethod bm_cert = m_cert.get_bytecodeMethod(object_level);
        MethodPolicy policy = bm_cert.policy();

        //Log.d("APKReader", "Checking" + "[" + object_level + "]" + policy.toString());
        /*System.out.print ("Checking ");
		System.out.print (m_cert.class_name() + m_cert.name() + ":" + m_cert.desc());
		System.out.println (" for policy ");
		System.out.println (policy.toString());*/

        if (bm.register_size < policy.localVariable_size()) {
            return new Result(false, "The number of local registers are less than the policy");
        }

    /* Sanity check: number of instructions and labels */
        if (bm_cert.ins_count() != bm.insns_count) {
            StringBuilder sb = new StringBuilder("Mismatch between numbers of codes and numbers of typing: ");
            sb.append(bm_cert.ins_count());
            sb.append(" != ");
            sb.append(bm.insns_count);
            return new Result(false, sb.toString());
        }

	/* TODO : need to check for r0 */

        int localN = policy.localVariable_size();
        CodeInterpreter.OpCode[] insnList = bm.insns;
        int insn_length = insnList.length;
        TreeMap<Integer, CodeInterpreter.OpCode> insnMap = bm.insnMap;
        for (int insn_idx = 0; insn_idx < insn_length; insn_idx++) {
            CodeInterpreter.OpCode ins = insnList[insn_idx];
            int label = ins.address;
            Typing typeInfo = bm_cert.get_type(label);
            //System.out.println ("Label : " + label);
            switch (ins.code) {
                case Nop: {
                    Typing succ_typing = bm_cert.get_type(label + ins.read_count);
                    if (!typeInfo.get_rt().leq(succ_typing.get_rt(), localN, lvl_rel))
                        return new Result(false, error_message(failedRT, label, "Nop"));
                    break;
                }
                case Move: {
                    Typing succ_typing = bm_cert.get_type(label + ins.read_count);
                    int r = ins.registers[0];
                    int rs = ins.registers[1];

                    // check the typability w.r.t the successor
                    BasicLevel se = typeInfo.get_se();
                    ExtendedLevel ks = typeInfo.get_rt_at(rs);
                    ExtendedLevel kr = ks.lub(ExtendedLevel.createSimple(se), lvl_rel);
                    Typing.RT rt = typeInfo.get_rt();
                    rt.put(r, kr);
                    if (!rt.leq(succ_typing.get_rt(), localN, lvl_rel))
                        return new Result(false, error_message(failedRT, label, "Move"));

                    // check the constraint if it is modifying register 0 or within local variable
                    if ((r == 0) && (insn_idx > 0)) {
                        if (!succ_typing.get_flag()) // modifying r_0 but succ_flag is still 0
                            return new Result(false, error_message(failedFlag, label, "Move"));
                    } else if (rs == 0) // using a value from r_0 when it is already used for return value
                    {
                        if (typeInfo.get_flag())
                            return new Result(false, error_message(failedFlag, label, "Move"));
                    } else if ((r > 0) && (r < localN)) // for the case where we should check the constraint because they are in the local variable section
                    {
                        if (!kr.leq(policy.ka_at(r), lvl_rel))
                            return new Result(false, error_message(failedConstraint, label, "Move"));
                    }
                    break;
                }
                case MoveResult: {
                    Typing succ_typing = bm_cert.get_type(label + ins.read_count);
                    int r = ins.registers[0];

                    // check the typability w.r.t the successor
                    BasicLevel se = typeInfo.get_se();
                    ExtendedLevel k_res = typeInfo.get_res();
                    ExtendedLevel kr = k_res.lub(ExtendedLevel.createSimple(se), lvl_rel);
                    Typing.RT rt = typeInfo.get_rt();
                    rt.put(r, kr);
                    if (!rt.leq(succ_typing.get_rt(), localN, lvl_rel))
                        return new Result(false, error_message(failedRT, label, "MoveResult"));

                    // check the constraint if it is modifying register 0 or within local variable
                    if ((r == 0) && (insn_idx > 0)) // checking the flag for r_0
                    {
                        if (!succ_typing.get_flag())
                            return new Result(false, error_message(failedFlag, label, "MoveResult"));
                    } else if ((r > 0) && (r < localN)) // checking constraint for the local variable section
                    {
                        if (!kr.leq(policy.ka_at(r), lvl_rel))
                            return new Result(false, error_message(failedConstraint, label, "MoveResult"));
                    }
                    break;
                }
                case Goto: {
                    Typing succ_typing = bm_cert.get_type((int) ins.literal);
                    if (!typeInfo.get_rt().leq(succ_typing.get_rt(), localN, lvl_rel))
                        return new Result(false, error_message(failedRT, label, "Goto"));
                    break;
                }
                case Const: {
                    Typing succ_typing = bm_cert.get_type(label + ins.read_count);
                    int register = ins.registers[0]; // we can assume that there is only one register
                    // check the typability w.r.t the successor
                    BasicLevel se = typeInfo.get_se();
                    Typing.RT rt = typeInfo.get_rt();
                    rt.put(register, ExtendedLevel.createSimple(se));
                    if (!rt.leq(succ_typing.get_rt(), localN, lvl_rel))
                        return new Result(false, error_message(failedRT, label, "Const"));

                    // check the constraint if it is modifying register 0 or it is within local variable
                    if ((register == 0) && (insn_idx > 0)) {
                        if (!succ_typing.get_flag())
                            return new Result(false, error_message(failedFlag, label, "Const"));
                    } else if ((register > 0) && (register < localN)) {
                        if (!ExtendedLevel.createSimple(se).leq(policy.ka_at(register), lvl_rel))
                            return new Result(false, error_message(failedConstraint, label, "Const"));
                    }
                    break;
                }
                case Binop: {
                    Typing succ_typing = bm_cert.get_type(label + ins.read_count);
                    int ra = ins.registers[0];
                    int rb = ins.registers[1];
                    int rc = ins.registers[2];
                    // check the typability w.r.t. the successor
                    BasicLevel se = typeInfo.get_se();
                    ExtendedLevel kb = typeInfo.get_rt_at(rb);
                    ExtendedLevel kc = typeInfo.get_rt_at(rc);
                    ExtendedLevel kr = kb.lub(kc.lub(ExtendedLevel.createSimple(se), lvl_rel), lvl_rel);
                    Typing.RT rt = typeInfo.get_rt();
                    rt.put(ra, kr);
                    if (!rt.leq(succ_typing.get_rt(), localN, lvl_rel))
                        return new Result(false, error_message(failedRT, label, "Binop"));

                    // check the constraint if it is modifying register 0 or within local variable
                    if ((ra == 0) && (insn_idx > 0)) // taking care of modifying r_0
                    {
                        if (!succ_typing.get_flag())
                            return new Result(false, error_message(failedFlag, label, "Binop"));
                    } else if ((rb == 0) || (rc == 0)) {
                        if (typeInfo.get_flag())
                            return new Result(false, error_message(failedFlag, label, "Binop"));
                    } else if ((ra > 0) && (ra < localN)) {
                        if (!kr.leq(policy.ka_at(ra), lvl_rel))
                            return new Result(false, error_message(failedConstraint, label, "Binop"));
                    }
                    break;
                }
                case Binop2addr: {
                    Typing succ_typing = bm_cert.get_type(label + ins.read_count);
                    int ra = ins.registers[0];
                    int rb = ins.registers[1];
                    // check the typability w.r.t. the successor
                    BasicLevel se = typeInfo.get_se();
                    ExtendedLevel ka = typeInfo.get_rt_at(ra);
                    ExtendedLevel kb = typeInfo.get_rt_at(rb);
                    ExtendedLevel kr = ka.lub(kb.lub(ExtendedLevel.createSimple(se), lvl_rel), lvl_rel);
                    Typing.RT rt = typeInfo.get_rt();
                    rt.put(ra, kr);
                    if (!rt.leq(succ_typing.get_rt(), localN, lvl_rel))
                        return new Result(false, error_message(failedRT, label, "Binop2addr"));

                    //check the constraint if it is modifying register 0 or within local variable
                    if ((ra == 0) && (insn_idx > 0)) {
                        if (!succ_typing.get_flag())
                            return new Result(false, error_message(failedFlag, label, "Binop2addr"));
                    } else if ((ra == 0) || (rb == 0)) {
                        if (typeInfo.get_flag())
                            return new Result(false, error_message(failedFlag, label, "Binop2addr"));
                    } else if ((ra > 0) && (ra < localN)) {
                        if (!kr.leq(policy.ka_at(ra), lvl_rel))
                            return new Result(false, error_message(failedConstraint, label, "Binop2addr"));
                    }
                    break;
                }
                case BinopConst: {
                    Typing succ_typing = bm_cert.get_type(label + ins.read_count);
                    int ra = ins.registers[0];
                    int rb = ins.registers[1];
                    // check the typability w.r.t. the successor
                    BasicLevel se = typeInfo.get_se();
                    ExtendedLevel kb = typeInfo.get_rt_at(rb);
                    ExtendedLevel kr = kb.lub(ExtendedLevel.createSimple(se), lvl_rel);
                    Typing.RT rt = typeInfo.get_rt();
                    rt.put(ra, kr);
                    if (!rt.leq(succ_typing.get_rt(), localN, lvl_rel))
                        return new Result(false, error_message(failedRT, label, "BinopConst"));

                    // check the constraint if it is modifying register 0 or within local variable
                    if ((ra == 0) && (insn_idx > 0)) {
                        if (!succ_typing.get_flag())
                            return new Result(false, error_message(failedFlag, label, "BinopConst"));
                    } else if (rb == 0) {
                        if (typeInfo.get_flag())
                            return new Result(false, error_message(failedFlag, label, "BinopConst"));
                    } else if ((ra > 0) && (ra < localN)) {
                        if (!kr.leq(policy.ka_at(ra), lvl_rel))
                            return new Result(false, error_message(failedConstraint, label, "BinopConst"));
                    }
                    break;
                }
                case If: {
                    Typing succ_typing1 = bm_cert.get_type(label + ins.read_count);
                    Typing succ_typing2 = bm_cert.get_type((int) ins.literal);
                    int ra = ins.registers[0];
                    if ((ra == 0) && (typeInfo.get_flag()))
                        return new Result(false, error_message(failedFlag, label, "If"));

                    // Check the typability w.r.t. the successors
                    BasicLevel se = typeInfo.get_se();
                    ExtendedLevel ka = typeInfo.get_rt_at(ra);
                    ExtendedLevel kr = ka.lub(ExtendedLevel.createSimple(se), lvl_rel);
                    if (ins.register_size == 2) {
                        int rb = ins.registers[1];
                        if ((rb == 0) && (typeInfo.get_flag()))
                            return new Result(false, error_message(failedFlag, label, "If"));
                        ExtendedLevel kb = typeInfo.get_rt_at(rb);
                        kr = kb.lub(kr, lvl_rel);
                    }
                    Typing.RT rt = typeInfo.get_rt();
                    rt.lift(kr.getBasicLevel(), localN, lvl_rel);

                    if (!rt.leq(succ_typing1.get_rt(), localN, lvl_rel))
                        return new Result(false, error_message(failedRT, label, "If with " + (label + ins.read_count)));
                    if (!rt.leq(succ_typing2.get_rt(), localN, lvl_rel))
                        return new Result(false, error_message(failedRT, label, "If with " + ins.literal));

                    // Check the constraint for the region
                    Integer[] region = m_cert.get_region(label);
                    for (Integer idx : region) {
                        BasicLevel se_j = bm_cert.get_type(idx).get_se();
                        if (!kr.leq(ExtendedLevel.createSimple(se_j), lvl_rel))
                            return new Result(false, error_message(failedSE, label,
                                    "(" + kr.toString() + " is not less or equal than " + se_j.toString() + ")"));
                    }
                    break;
                }
                case Compare: {
                    Typing succ_typing = bm_cert.get_type(label + ins.read_count);
                    int ra = ins.registers[0];
                    int rb = ins.registers[1];
                    int rc = ins.registers[2];
                    // check the typability w.r.t. the successor
                    BasicLevel se = typeInfo.get_se();
                    ExtendedLevel kb = typeInfo.get_rt_at(rb);
                    ExtendedLevel kc = typeInfo.get_rt_at(rc);
                    ExtendedLevel kr = kb.lub(kc.lub(ExtendedLevel.createSimple(se), lvl_rel), lvl_rel);
                    Typing.RT rt = typeInfo.get_rt();
                    rt.put(ra, kr);
                    if (!rt.leq(succ_typing.get_rt(), localN, lvl_rel))
                        return new Result(false, error_message(failedRT, label, "Compare"));

                    // check the constraint if it is modifying register 0 or within local variable
                    if ((ra == 0) && (insn_idx > 0)) {
                        if (!succ_typing.get_flag())
                            return new Result(false, error_message(failedFlag, label, "Compare"));
                    } else if ((rb == 0) || (rc == 0)) {
                        if (typeInfo.get_flag())
                            return new Result(false, error_message(failedFlag, label, "Compare"));
                    } else if ((ra > 0) && (ra < localN)) {
                        if (!kr.leq(policy.ka_at(ra), lvl_rel))
                            return new Result(false, error_message(failedConstraint, label, "Compare"));
                    }
                    break;
                }
                case Return: {
                    if (ins.register_size == 0) break;
	      /* TODO: the paper assume we have return value, so for now we bypass any return void */
                    int r = ins.registers[0];
                    BasicLevel se = typeInfo.get_se();
                    ExtendedLevel kr = typeInfo.get_rt_at(r);
                    ExtendedLevel k = kr.lub(ExtendedLevel.createSimple(se), lvl_rel);
                    ExtendedLevel kr_n = policy.kr_at(0);
                    if (!k.leq(kr_n, lvl_rel))
                        return new Result(false, error_message(failedConstraint, label, "Return"));
                    break;
                }
	    /* TODO : for now we don't use PackedSwitch and SparseSwitch */
                case Unop: {
                    Typing succ_typing = bm_cert.get_type(label + ins.read_count);
                    int ra = ins.registers[0];
                    int rb = ins.registers[1];
                    BasicLevel se = typeInfo.get_se();
                    ExtendedLevel kb = typeInfo.get_rt_at(rb);
                    ExtendedLevel kr = kb.lub(ExtendedLevel.createSimple(se), lvl_rel);
                    Typing.RT rt = typeInfo.get_rt();
                    rt.put(ra, kr);
                    if (!rt.leq(succ_typing.get_rt(), localN, lvl_rel))
                        return new Result(false, error_message(failedRT, label, "Unop"));

                    // check the constraint if it is modifying register 0 or within local variable
                    if ((ra == 0) && (insn_idx > 0)) {
                        if (!succ_typing.get_flag())
                            return new Result(false, error_message(failedFlag, label, "Unop"));
                    } else if (rb == 0) {
                        if (typeInfo.get_flag())
                            return new Result(false, error_message(failedFlag, label, "Unop"));
                    } else if ((ra > 0) && (ra < localN)) {
                        if (!kr.leq(policy.ka_at(ra), lvl_rel))
                            return new Result(false, error_message(failedConstraint, label, "Unop"));
                    }
                    break;
                }
                case Invoke: { // use a naive assumption where there will always be at least one register for
                    // a reference to this
                    Typing succ_typing = bm_cert.get_type(label + ins.read_count);
                    BasicLevel se = typeInfo.get_se();
                    BasicLevel kh = bm_cert.policy().kh();

                    int ro = ins.registers[0];
                    MethodCert target = _cert.method(ins.method_item.class_str, ins.method_item.name_str,
                            ins.method_item.proto_item.shorty_desc);

                    ExtendedLevel ko = typeInfo.get_rt_at(ro);
                    MethodPolicy target_lvt = target.get_bytecodeMethod(ko.getBasicLevel().id()).policy();
                    BasicLevel target_kh = target_lvt.kh();

                    Typing.RT rt = typeInfo.get_rt();
	      /* checking the type registers (except the result register) */
                    //Log.d("APKReader", label + " " + rt.toString());
                    //Log.d("APKReader", label + " " + succ_typing.get_rt().toString());
                    if (!rt.leq(succ_typing.get_rt(), localN, lvl_rel))
                        return new Result(false, error_message(failedRT, label, "Invoke"));

	      /* checking the result register if there is a return value */
                    if (target_lvt.returnType_size() > 0) {
                        ExtendedLevel target_kr = target_lvt.kr_at(0); /* kr[n] will always be the first */
                        ExtendedLevel res = ExtendedLevel.createSimple(se).lub(target_kr, lvl_rel);
                        ExtendedLevel expected_res = succ_typing.get_res();
						/*if (label == 6) {
							Log.d("APKReader", rt.toString());
							Log.d("APKReader", ins.method_item.class_str + ins.method_item.	name_str +
									ins.method_item.proto_item.shorty_desc);
							Log.d("APKReader", ins.registers[0] + "[" + ko.toString() + "]" + target_lvt.toString() + " -> " + target_kr.toString());
							Log.d("APKReader", target_kr.toString() + " " + se.toString());
							Log.d("APKReader", (label + ": Invoke " + res.toString() + " - " + expected_res.toString() + " - " + Boolean.toString(res.leq(expected_res, lvl_rel))));
						}*/
                        if (!res.leq(expected_res, lvl_rel))
                            return new Result(false, error_message("Failed checking result register at ", label, "Invoke"));
                    }

                    // check the constraint if it is modifying register 0 or within local variable
                    // forall_i sec(p[i]) <= ka'(i)
                    boolean constraint1 = true;
                    for (int i = 0; i < ins.register_size; i++) {
                        ExtendedLevel ki = typeInfo.get_rt_at(ins.registers[i]);
                        constraint1 &= ki.leq(target_lvt.ka_at(i), lvl_rel);
                        if ((ins.registers[i] == 0) && (typeInfo.get_flag()))
                            return new Result(false, error_message(failedFlag, label, "Invoke"));
                    }

                    // kh U se U ko <= kh'
                    ExtendedLevel k2 = ExtendedLevel.createSimple(kh.lub(se, lvl_rel)).lub(ko, lvl_rel);
                    boolean constraint2 = k2.getBasicLevel().leq(target_kh, lvl_rel);
                    if (!(constraint1 && constraint2))
                        return new Result(false, error_message(failedConstraint, label, "Invoke"));

                    break;
                }
                case Iget: {
                    Typing succ_typing = bm_cert.get_type(label + ins.read_count);
                    int ro = ins.registers[0];
                    int r = ins.registers[1];
                    String class_name = ins.field_item.class_str;
                    String name = ins.field_item.name_str;

                    // check the typability w.r.t the successor
                    BasicLevel se = typeInfo.get_se();
                    ExtendedLevel ko = typeInfo.get_rt_at(ro);
                    ExtendedLevel ft = _cert.ft().get(class_name, name);
                    ExtendedLevel kr = (ko.lub(ExtendedLevel.createSimple(se), lvl_rel)).lub(ft, lvl_rel);
                    Typing.RT rt = typeInfo.get_rt();
                    rt.put(r, kr);
                    if (!rt.leq(succ_typing.get_rt(), localN, lvl_rel))
                        return new Result(false, error_message(failedRT, label, "Iget"));

                    // check the constraint if it is modifying register 0 or within local variable
                    if ((r == 0) && (insn_idx > 0)) {
                        if (!succ_typing.get_flag())
                            return new Result(false, error_message(failedFlag, label, "Iget"));
                    } else if (ro == 0) {
                        if (typeInfo.get_flag())
                            return new Result(false, error_message(failedFlag, label, "Iget"));
                    } else if ((r > 0) && (r < localN)) {
                        if (!kr.leq(policy.ka_at(r), lvl_rel))
                            return new Result(false, error_message(failedConstraint, label, "Iget"));
                    }
                    break;
                }
                case Iput: {
                    Typing succ_typing = bm_cert.get_type(label + ins.read_count);
                    int ro = ins.registers[0];
                    int r = ins.registers[1];
                    String class_name = ins.field_item.class_str;
                    String name = ins.field_item.name_str;

                    // check the typability w.r.t the successor
                    BasicLevel se = typeInfo.get_se();
                    ExtendedLevel ko = typeInfo.get_rt_at(ro);
                    ExtendedLevel k = typeInfo.get_rt_at(r);
                    ExtendedLevel ft = _cert.ft().get(class_name, name);
                    ExtendedLevel kr = (ko.lub(ExtendedLevel.createSimple(se), lvl_rel)).lub(k, lvl_rel);
                    BasicLevel kh = bm_cert.policy().kh();
                    Typing.RT rt = typeInfo.get_rt();
                    if (!rt.leq(succ_typing.get_rt(), localN, lvl_rel))
                        return new Result(false, error_message(failedRT, label, "Iput"));

                    if ((r == 0) && (ro == 0)) {
                        if (typeInfo.get_flag())
                            return new Result(false, error_message(failedFlag, label, "Iput"));
                    }

                    boolean constraint1 = kr.leq(ft, lvl_rel);
                    boolean constraint2 = (ExtendedLevel.createSimple(kh)).leq(ft, lvl_rel);

                    if (!(constraint1 && constraint2))
                        return new Result(false, error_message(failedConstraint, label, "Iput"));

                    break;
                }
                case CheckCast: /* TODO : for now we treat CheckCast just like NOP */ {
                    Typing succ_typing = bm_cert.get_type(label + ins.read_count);
                    if (!typeInfo.get_rt().leq(succ_typing.get_rt(), localN, lvl_rel))
                        return new Result(false, error_message(failedRT, label, "CheckCast"));
                    break;
                }
                case NewInstance: {
                    Typing succ_typing = bm_cert.get_type(label + ins.read_count);
                    int register = ins.registers[0]; // we can assume that there is only one register
                    // check the typability w.r.t the successor
                    BasicLevel se = typeInfo.get_se();
                    Typing.RT rt = typeInfo.get_rt();
                    rt.put(register, ExtendedLevel.createSimple(se));
                    if (!rt.leq(succ_typing.get_rt(), localN, lvl_rel))
                        return new Result(false, error_message(failedRT, label, "NewInstance"));

                    // check the constraint if it is modifying register 0 or it is within local variable
                    if (register == 0) {
                        if (!succ_typing.get_flag())
                            return new Result(false, error_message(failedFlag, label, "NewInstance"));
                    } else if ((register > 0) && (register < localN)) {
                        if (!ExtendedLevel.createSimple(se).leq(policy.ka_at(register), lvl_rel))
                            return new Result(false, error_message(failedConstraint, label, "NewInstance"));
                    }
                    break;
                }
                default: /* Incomplete implementation for now */
                    return new Result(false, "Instruction not implemented yet :" + ins);
            }
        }
        return new Result(true, "Finished type checking without problem");
    }

    private boolean skipMethods(String class_name, String method_name) {
        // skipping basic Android and Java classes, and default classes
        if (class_name.startsWith("Landroid")) return true;
        if (class_name.startsWith("Ljava")) return true;
        if (class_name.endsWith("BuildConfig;")) return true;
        if (class_name.endsWith("R;")) return true;
        if (class_name.endsWith("R$anim;")) return true;
        if (class_name.endsWith("R$attr;")) return true;
        if (class_name.endsWith("R$bool;")) return true;
        if (class_name.endsWith("R$color;")) return true;
        if (class_name.endsWith("R$dimen;")) return true;
        if (class_name.endsWith("R$drawable;")) return true;
        if (class_name.endsWith("R$id;")) return true;
        if (class_name.endsWith("R$integer;")) return true;
        if (class_name.endsWith("R$layout;")) return true;
        if (class_name.endsWith("R$mipmap;")) return true;
        if (class_name.endsWith("R$string;")) return true;
        if (class_name.endsWith("R$style;")) return true;
        if (class_name.endsWith("R$styleable;")) return true;
        return false;
    }

    public Result typeCheck() {
        // for all method
        //   there is a method with the same name and signature both in file and cert
        //   with matching region, junction, se, and rt
        //   and these rt also typecheck

	/* skip the for all method check for now, just check that the name and signature is correct */
        ClassContainer cc = _file.cc;
        for (int i = 0; i < cc.size; i++) {
            ClassItem cdi = cc.items[i];
            int n_virtual = cdi.class_data.virtual_methods_size;
	  /* do this for all the other method */
            for (int j = 0; j < n_virtual; j++) {
		/* Sanity check: name check */
                String class_name = cdi.class_str;
                String method_name = cdi.class_data.virtual_methods[j].method.name_str;
                String method_desc = cdi.class_data.virtual_methods[j].method.proto_item.shorty_desc;
                // skipping basic Android and Java classes, and default classes
                if (skipMethods(class_name, method_name)) continue;
                //Log.d("APKReader", "Virtual Methods");
                if (!_cert.containMethod(class_name, method_name, method_desc)) {
                    return new Result(false,
                            "No matching method for " + class_name + method_name + method_desc);
                }

                MethodCert mc = _cert.method(class_name, method_name, method_desc);
                CodeItem code = cdi.class_data.virtual_methods[j].code;
                LevelPool lvl_pool = _cert.lvl_pool();
		/* Sanity check: enough local variables for the policy and return type */

                for (BasicLevel x : lvl_pool.get_contents()) {
                    // traverse the instructions (per object level) and do type checking
                    Result res = traverseBytecode_perLevel(code, mc, _cert.lvl_rel(), x.id());
                    if (!res.success()) return res;
                    //if (!res.success()) {System.out.println(res.comment());return false;}
                }
            }

            int n_direct = cdi.class_data.direct_methods_size;
	  /* do this for all the other method */
            for (int j = 0; j < n_direct; j++) {
		/* Sanity check: name check */
                String class_name = cdi.class_str;
                String method_name = cdi.class_data.direct_methods[j].method.name_str;
                String method_desc = cdi.class_data.direct_methods[j].method.proto_item.shorty_desc;

                // skipping basic Android and Java classes, and default classes
                if (skipMethods(class_name, method_name)) continue;
                //Log.d("APKReader", "Direct Methods");
                if (!_cert.containMethod(class_name, method_name, method_desc)) {
                    return new Result(false,
                            "No matching method for " + class_name + method_name + method_desc);
                    //return false;
                }

                MethodCert mc = _cert.method(class_name, method_name, method_desc);
                CodeItem code = cdi.class_data.direct_methods[j].code;
                LevelPool lvl_pool = _cert.lvl_pool();
		/* Sanity check: enough local variables for the policy and return type */

                for (BasicLevel x : lvl_pool.get_contents()) {
                    // traverse the instructions (per object level) and do type checking
                    Result res = traverseBytecode_perLevel(code, mc, _cert.lvl_rel(), x.id());
                    if (!res.success()) return res;
                    //if (!res.success()) {System.out.println(res.comment()); return false;}
                }
            }
        }
        //Log.d("APKReader", "No comment but false?");
        return new Result(true, "No Comment");
    }

    public static class Result {
        private boolean _success;
        private String _comment;

        public Result(boolean success, String comment) {
            _success = success;
            _comment = comment;
        }

        public boolean success() {
            return _success;
        }

        public String comment() {
            return _comment;
        }
    }
}
