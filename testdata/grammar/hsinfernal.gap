import rational
import choener

type str = extern

signature Algebra(alphabet, answer) {
answer f_IL_1(rational,alphabet,answer);
answer f_IR_2(rational,answer,alphabet);
answer f_MP_3(rational,alphabet,answer,alphabet);
answer f_ML_4(rational,alphabet,answer);
answer f_MR_5(rational,answer,alphabet);
answer f_D_6(rational,answer);
answer f_IL_7(rational,alphabet,answer);
answer f_IR_8(rational,answer,alphabet);
answer f_MP_9(rational,alphabet,answer,alphabet);
answer f_ML_10(rational,alphabet,answer);
answer f_MR_11(rational,answer,alphabet);
answer f_D_12(rational,answer);
answer f_IL_13(rational,alphabet,answer);
answer f_IR_14(rational,answer,alphabet);
answer f_MP_15(rational,alphabet,answer,alphabet);
answer f_ML_16(rational,alphabet,answer);
answer f_MR_17(rational,answer,alphabet);
answer f_D_18(rational,answer);
answer f_IL_19(rational,alphabet,answer);
answer f_IR_20(rational,answer,alphabet);
answer f_MP_21(rational,alphabet,answer,alphabet);
answer f_ML_22(rational,alphabet,answer);
answer f_MR_23(rational,answer,alphabet);
answer f_D_24(rational,answer);
answer f_IL_25(rational,alphabet,answer);
answer f_IR_26(rational,answer,alphabet);
answer f_MP_27(rational,alphabet,answer,alphabet);
answer f_ML_28(rational,alphabet,answer);
answer f_MR_29(rational,answer,alphabet);
answer f_D_30(rational,answer);
answer f_IL_31(rational,alphabet,answer);
answer f_IR_32(rational,answer,alphabet);
answer f_MP_33(rational,alphabet,answer,alphabet);
answer f_ML_34(rational,alphabet,answer);
answer f_MR_35(rational,answer,alphabet);
answer f_D_36(rational,answer);
answer f_IL_37(rational,alphabet,answer);
answer f_IR_38(rational,answer,alphabet);
answer f_ML_39(rational,alphabet,answer);
answer f_D_40(rational,answer);
answer f_IL_41(rational,alphabet,answer);
answer f_ML_42(rational,alphabet,answer);
answer f_D_43(rational,answer);
answer f_IL_44(rational,alphabet,answer);
answer f_ML_45(rational,alphabet,answer);
answer f_D_46(rational,answer);
answer f_IL_47(rational,alphabet,answer);
answer f_ML_48(rational,alphabet,answer);
answer f_D_49(rational,answer);
answer f_IL_50(rational,alphabet,answer);
answer f_ML_51(rational,alphabet,answer);
answer f_D_52(rational,answer);
answer f_IL_53(rational,alphabet,answer);
answer f_ML_54(rational,alphabet,answer);
answer f_D_55(rational,answer);
answer f_IL_56(rational,alphabet,answer);
answer f_ML_57(rational,alphabet,answer);
answer f_D_58(rational,answer);
answer f_IL_59(rational,alphabet,answer);
answer f_MR_60(rational,answer,alphabet);
answer f_D_61(rational,answer);
answer f_IR_62(rational,answer,alphabet);
answer f_B_63(answer,answer);
answer f_S_64(rational,answer);
answer f_IL_65(rational,alphabet,answer);
answer f_ML_66(rational,alphabet,answer);
answer f_D_67(rational,answer);
answer f_IL_68(rational,alphabet,answer);
answer f_ML_69(rational,alphabet,answer);
answer f_D_70(rational,answer);
answer f_IL_71(rational,alphabet,answer);
answer f_ML_72(rational,alphabet,answer);
answer f_D_73(rational,answer);
answer f_IL_74(rational,alphabet,answer);
answer f_MP_75(rational,alphabet,answer,alphabet);
answer f_ML_76(rational,alphabet,answer);
answer f_MR_77(rational,answer,alphabet);
answer f_D_78(rational,answer);
answer f_IL_79(rational,alphabet,answer);
answer f_IR_80(rational,answer,alphabet);
answer f_MP_81(rational,alphabet,answer,alphabet);
answer f_ML_82(rational,alphabet,answer);
answer f_MR_83(rational,answer,alphabet);
answer f_D_84(rational,answer);
answer f_IL_85(rational,alphabet,answer);
answer f_IR_86(rational,answer,alphabet);
answer f_MP_87(rational,alphabet,answer,alphabet);
answer f_ML_88(rational,alphabet,answer);
answer f_MR_89(rational,answer,alphabet);
answer f_D_90(rational,answer);
answer f_IL_91(rational,alphabet,answer);
answer f_IR_92(rational,answer,alphabet);
answer f_MP_93(rational,alphabet,answer,alphabet);
answer f_ML_94(rational,alphabet,answer);
answer f_MR_95(rational,answer,alphabet);
answer f_D_96(rational,answer);
answer f_E_99(rational,answer);
answer f_S_100(rational,answer);
answer f_MP_101(rational,alphabet,answer,alphabet);
answer f_ML_102(rational,alphabet,answer);
answer f_MR_103(rational,answer,alphabet);
answer f_D_104(rational,answer);
answer f_IL_105(rational,alphabet,answer);
answer f_IR_106(rational,answer,alphabet);
answer f_MP_107(rational,alphabet,answer,alphabet);
answer f_ML_108(rational,alphabet,answer);
answer f_MR_109(rational,answer,alphabet);
answer f_D_110(rational,answer);
answer f_IL_111(rational,alphabet,answer);
answer f_IR_112(rational,answer,alphabet);
answer f_MP_113(rational,alphabet,answer,alphabet);
answer f_ML_114(rational,alphabet,answer);
answer f_MR_115(rational,answer,alphabet);
answer f_D_116(rational,answer);
answer f_IL_117(rational,alphabet,answer);
answer f_IR_118(rational,answer,alphabet);
answer f_MP_119(rational,alphabet,answer,alphabet);
answer f_ML_120(rational,alphabet,answer);
answer f_MR_121(rational,answer,alphabet);
answer f_D_122(rational,answer);
answer f_IL_123(rational,alphabet,answer);
answer f_IR_124(rational,answer,alphabet);
answer f_ML_125(rational,alphabet,answer);
answer f_D_126(rational,answer);
answer f_IL_127(rational,alphabet,answer);
answer f_ML_128(rational,alphabet,answer);
answer f_D_129(rational,answer);
answer f_IL_130(rational,alphabet,answer);
answer f_ML_131(rational,alphabet,answer);
answer f_D_132(rational,answer);
answer f_IL_133(rational,alphabet,answer);
answer f_ML_134(rational,alphabet,answer);
answer f_D_135(rational,answer);
answer f_IL_136(rational,alphabet,answer);
answer f_ML_137(rational,alphabet,answer);
answer f_D_138(rational,answer);
answer f_IL_139(rational,alphabet,answer);
answer f_ML_140(rational,alphabet,answer);
answer f_D_141(rational,answer);
answer f_E_143(rational,answer);
answer nil(void);
choice [answer] h([answer]);
}

algebra count auto count;
algebra enum auto enum;
algebra Ambi implements Algebra(alphabet = char, answer = str) {
     str f_IL_1(rational p,  char b,  str s) {  str ret; append(ret, 'I'); append(ret, s); return ret; }
     str f_IR_2(rational p,  str s,  char b) {  return pushIR(s); }
     str f_MP_3(rational p,  char a,  str s,  char b) {  str ret; append(ret, 'P'); append(ret, s); append(ret, 'K'); return ret; }
     str f_ML_4(rational p,  char b,  str s) {  str ret; append(ret, 'L'); append(ret, s); append(ret, 'r'); return ret; }
     str f_MR_5(rational p,  str s,  char b) {  str ret; append(ret, 'l'); append(ret, s); append(ret, 'R'); return ret; }
     str f_D_6(rational p,  str s) {  str ret; append(ret, 'l'); append(ret, s); append(ret, 'r'); return ret; }
     str f_IL_7(rational p,  char b,  str s) {  str ret; append(ret, 'I'); append(ret, s); return ret; }
     str f_IR_8(rational p,  str s,  char b) {  return pushIR(s); }
     str f_MP_9(rational p,  char a,  str s,  char b) {  str ret; append(ret, 'P'); append(ret, s); append(ret, 'K'); return ret; }
     str f_ML_10(rational p,  char b,  str s) {  str ret; append(ret, 'L'); append(ret, s); append(ret, 'r'); return ret; }
     str f_MR_11(rational p,  str s,  char b) {  str ret; append(ret, 'l'); append(ret, s); append(ret, 'R'); return ret; }
     str f_D_12(rational p,  str s) {  str ret; append(ret, 'l'); append(ret, s); append(ret, 'r'); return ret; }
     str f_IL_13(rational p,  char b,  str s) {  str ret; append(ret, 'I'); append(ret, s); return ret; }
     str f_IR_14(rational p,  str s,  char b) {  return pushIR(s); }
     str f_MP_15(rational p,  char a,  str s,  char b) {  str ret; append(ret, 'P'); append(ret, s); append(ret, 'K'); return ret; }
     str f_ML_16(rational p,  char b,  str s) {  str ret; append(ret, 'L'); append(ret, s); append(ret, 'r'); return ret; }
     str f_MR_17(rational p,  str s,  char b) {  str ret; append(ret, 'l'); append(ret, s); append(ret, 'R'); return ret; }
     str f_D_18(rational p,  str s) {  str ret; append(ret, 'l'); append(ret, s); append(ret, 'r'); return ret; }
     str f_IL_19(rational p,  char b,  str s) {  str ret; append(ret, 'I'); append(ret, s); return ret; }
     str f_IR_20(rational p,  str s,  char b) {  return pushIR(s); }
     str f_MP_21(rational p,  char a,  str s,  char b) {  str ret; append(ret, 'P'); append(ret, s); append(ret, 'K'); return ret; }
     str f_ML_22(rational p,  char b,  str s) {  str ret; append(ret, 'L'); append(ret, s); append(ret, 'r'); return ret; }
     str f_MR_23(rational p,  str s,  char b) {  str ret; append(ret, 'l'); append(ret, s); append(ret, 'R'); return ret; }
     str f_D_24(rational p,  str s) {  str ret; append(ret, 'l'); append(ret, s); append(ret, 'r'); return ret; }
     str f_IL_25(rational p,  char b,  str s) {  str ret; append(ret, 'I'); append(ret, s); return ret; }
     str f_IR_26(rational p,  str s,  char b) {  return pushIR(s); }
     str f_MP_27(rational p,  char a,  str s,  char b) {  str ret; append(ret, 'P'); append(ret, s); append(ret, 'K'); return ret; }
     str f_ML_28(rational p,  char b,  str s) {  str ret; append(ret, 'L'); append(ret, s); append(ret, 'r'); return ret; }
     str f_MR_29(rational p,  str s,  char b) {  str ret; append(ret, 'l'); append(ret, s); append(ret, 'R'); return ret; }
     str f_D_30(rational p,  str s) {  str ret; append(ret, 'l'); append(ret, s); append(ret, 'r'); return ret; }
     str f_IL_31(rational p,  char b,  str s) {  str ret; append(ret, 'I'); append(ret, s); return ret; }
     str f_IR_32(rational p,  str s,  char b) {  return pushIR(s); }
     str f_MP_33(rational p,  char a,  str s,  char b) {  str ret; append(ret, 'P'); append(ret, s); append(ret, 'K'); return ret; }
     str f_ML_34(rational p,  char b,  str s) {  str ret; append(ret, 'L'); append(ret, s); append(ret, 'r'); return ret; }
     str f_MR_35(rational p,  str s,  char b) {  str ret; append(ret, 'l'); append(ret, s); append(ret, 'R'); return ret; }
     str f_D_36(rational p,  str s) {  str ret; append(ret, 'l'); append(ret, s); append(ret, 'r'); return ret; }
     str f_IL_37(rational p,  char b,  str s) {  str ret; append(ret, 'I'); append(ret, s); return ret; }
     str f_IR_38(rational p,  str s,  char b) {  return pushIR(s); }
     str f_ML_39(rational p,  char b,  str s) {  str ret; append(ret, 'M'); append(ret, s); return ret; }
     str f_D_40(rational p,  str s) {  return pushD(s); }
     str f_IL_41(rational p,  char b,  str s) {  str ret; append(ret, 'I'); append(ret, s); return ret; }
     str f_ML_42(rational p,  char b,  str s) {  str ret; append(ret, 'M'); append(ret, s); return ret; }
     str f_D_43(rational p,  str s) {  return pushD(s); }
     str f_IL_44(rational p,  char b,  str s) {  str ret; append(ret, 'I'); append(ret, s); return ret; }
     str f_ML_45(rational p,  char b,  str s) {  str ret; append(ret, 'M'); append(ret, s); return ret; }
     str f_D_46(rational p,  str s) {  return pushD(s); }
     str f_IL_47(rational p,  char b,  str s) {  str ret; append(ret, 'I'); append(ret, s); return ret; }
     str f_ML_48(rational p,  char b,  str s) {  str ret; append(ret, 'M'); append(ret, s); return ret; }
     str f_D_49(rational p,  str s) {  return pushD(s); }
     str f_IL_50(rational p,  char b,  str s) {  str ret; append(ret, 'I'); append(ret, s); return ret; }
     str f_ML_51(rational p,  char b,  str s) {  str ret; append(ret, 'M'); append(ret, s); return ret; }
     str f_D_52(rational p,  str s) {  return pushD(s); }
     str f_IL_53(rational p,  char b,  str s) {  str ret; append(ret, 'I'); append(ret, s); return ret; }
     str f_ML_54(rational p,  char b,  str s) {  str ret; append(ret, 'M'); append(ret, s); return ret; }
     str f_D_55(rational p,  str s) {  return pushD(s); }
     str f_IL_56(rational p,  char b,  str s) {  str ret; append(ret, 'I'); append(ret, s); return ret; }
     str f_ML_57(rational p,  char b,  str s) {  str ret; append(ret, 'M'); append(ret, s); return ret; }
     str f_D_58(rational p,  str s) {  return pushD(s); }
     str f_IL_59(rational p,  char b,  str s) {  str ret; append(ret, 'I'); append(ret, s); return ret; }
     str f_MR_60(rational p,  str s,  char b) {  str ret; append(ret, s); append(ret, 'M'); return ret; }
     str f_D_61(rational p,  str s) {  return pushD(s); }
     str f_IR_62(rational p,  str s,  char b) {  return pushIR(s); }
     str f_B_63(str l,  str r) {  str ret; append(ret, l); append(ret, r); return ret; }
     str f_S_64(rational p,  str s) { return s; }
     str f_IL_65(rational p,  char b,  str s) {  str ret; append(ret, 'I'); append(ret, s); return ret; }
     str f_ML_66(rational p,  char b,  str s) {  str ret; append(ret, 'M'); append(ret, s); return ret; }
     str f_D_67(rational p,  str s) {  return pushD(s); }
     str f_IL_68(rational p,  char b,  str s) {  str ret; append(ret, 'I'); append(ret, s); return ret; }
     str f_ML_69(rational p,  char b,  str s) {  str ret; append(ret, 'M'); append(ret, s); return ret; }
     str f_D_70(rational p,  str s) {  return pushD(s); }
     str f_IL_71(rational p,  char b,  str s) {  str ret; append(ret, 'I'); append(ret, s); return ret; }
     str f_ML_72(rational p,  char b,  str s) {  str ret; append(ret, 'M'); append(ret, s); return ret; }
     str f_D_73(rational p,  str s) {  return pushD(s); }
     str f_IL_74(rational p,  char b,  str s) {  str ret; append(ret, 'I'); append(ret, s); return ret; }
     str f_MP_75(rational p,  char a,  str s,  char b) {  str ret; append(ret, 'P'); append(ret, s); append(ret, 'K'); return ret; }
     str f_ML_76(rational p,  char b,  str s) {  str ret; append(ret, 'L'); append(ret, s); append(ret, 'r'); return ret; }
     str f_MR_77(rational p,  str s,  char b) {  str ret; append(ret, 'l'); append(ret, s); append(ret, 'R'); return ret; }
     str f_D_78(rational p,  str s) {  str ret; append(ret, 'l'); append(ret, s); append(ret, 'r'); return ret; }
     str f_IL_79(rational p,  char b,  str s) {  str ret; append(ret, 'I'); append(ret, s); return ret; }
     str f_IR_80(rational p,  str s,  char b) {  return pushIR(s); }
     str f_MP_81(rational p,  char a,  str s,  char b) {  str ret; append(ret, 'P'); append(ret, s); append(ret, 'K'); return ret; }
     str f_ML_82(rational p,  char b,  str s) {  str ret; append(ret, 'L'); append(ret, s); append(ret, 'r'); return ret; }
     str f_MR_83(rational p,  str s,  char b) {  str ret; append(ret, 'l'); append(ret, s); append(ret, 'R'); return ret; }
     str f_D_84(rational p,  str s) {  str ret; append(ret, 'l'); append(ret, s); append(ret, 'r'); return ret; }
     str f_IL_85(rational p,  char b,  str s) {  str ret; append(ret, 'I'); append(ret, s); return ret; }
     str f_IR_86(rational p,  str s,  char b) {  return pushIR(s); }
     str f_MP_87(rational p,  char a,  str s,  char b) {  str ret; append(ret, 'P'); append(ret, s); append(ret, 'K'); return ret; }
     str f_ML_88(rational p,  char b,  str s) {  str ret; append(ret, 'L'); append(ret, s); append(ret, 'r'); return ret; }
     str f_MR_89(rational p,  str s,  char b) {  str ret; append(ret, 'l'); append(ret, s); append(ret, 'R'); return ret; }
     str f_D_90(rational p,  str s) {  str ret; append(ret, 'l'); append(ret, s); append(ret, 'r'); return ret; }
     str f_IL_91(rational p,  char b,  str s) {  str ret; append(ret, 'I'); append(ret, s); return ret; }
     str f_IR_92(rational p,  str s,  char b) {  return pushIR(s); }
     str f_MP_93(rational p,  char a,  str s,  char b) {  str ret; append(ret, 'P'); append(ret, s); append(ret, 'K'); return ret; }
     str f_ML_94(rational p,  char b,  str s) {  str ret; append(ret, 'L'); append(ret, s); append(ret, 'r'); return ret; }
     str f_MR_95(rational p,  str s,  char b) {  str ret; append(ret, 'l'); append(ret, s); append(ret, 'R'); return ret; }
     str f_D_96(rational p,  str s) {  str ret; append(ret, 'l'); append(ret, s); append(ret, 'r'); return ret; }
     str f_E_99(rational p,  str s) {  str ret; return ret; }
     str f_S_100(rational p,  str s) { return s; }
     str f_MP_101(rational p,  char a,  str s,  char b) {  str ret; append(ret, 'P'); append(ret, s); append(ret, 'K'); return ret; }
     str f_ML_102(rational p,  char b,  str s) {  str ret; append(ret, 'L'); append(ret, s); append(ret, 'r'); return ret; }
     str f_MR_103(rational p,  str s,  char b) {  str ret; append(ret, 'l'); append(ret, s); append(ret, 'R'); return ret; }
     str f_D_104(rational p,  str s) {  str ret; append(ret, 'l'); append(ret, s); append(ret, 'r'); return ret; }
     str f_IL_105(rational p,  char b,  str s) {  str ret; append(ret, 'I'); append(ret, s); return ret; }
     str f_IR_106(rational p,  str s,  char b) {  return pushIR(s); }
     str f_MP_107(rational p,  char a,  str s,  char b) {  str ret; append(ret, 'P'); append(ret, s); append(ret, 'K'); return ret; }
     str f_ML_108(rational p,  char b,  str s) {  str ret; append(ret, 'L'); append(ret, s); append(ret, 'r'); return ret; }
     str f_MR_109(rational p,  str s,  char b) {  str ret; append(ret, 'l'); append(ret, s); append(ret, 'R'); return ret; }
     str f_D_110(rational p,  str s) {  str ret; append(ret, 'l'); append(ret, s); append(ret, 'r'); return ret; }
     str f_IL_111(rational p,  char b,  str s) {  str ret; append(ret, 'I'); append(ret, s); return ret; }
     str f_IR_112(rational p,  str s,  char b) {  return pushIR(s); }
     str f_MP_113(rational p,  char a,  str s,  char b) {  str ret; append(ret, 'P'); append(ret, s); append(ret, 'K'); return ret; }
     str f_ML_114(rational p,  char b,  str s) {  str ret; append(ret, 'L'); append(ret, s); append(ret, 'r'); return ret; }
     str f_MR_115(rational p,  str s,  char b) {  str ret; append(ret, 'l'); append(ret, s); append(ret, 'R'); return ret; }
     str f_D_116(rational p,  str s) {  str ret; append(ret, 'l'); append(ret, s); append(ret, 'r'); return ret; }
     str f_IL_117(rational p,  char b,  str s) {  str ret; append(ret, 'I'); append(ret, s); return ret; }
     str f_IR_118(rational p,  str s,  char b) {  return pushIR(s); }
     str f_MP_119(rational p,  char a,  str s,  char b) {  str ret; append(ret, 'P'); append(ret, s); append(ret, 'K'); return ret; }
     str f_ML_120(rational p,  char b,  str s) {  str ret; append(ret, 'L'); append(ret, s); append(ret, 'r'); return ret; }
     str f_MR_121(rational p,  str s,  char b) {  str ret; append(ret, 'l'); append(ret, s); append(ret, 'R'); return ret; }
     str f_D_122(rational p,  str s) {  str ret; append(ret, 'l'); append(ret, s); append(ret, 'r'); return ret; }
     str f_IL_123(rational p,  char b,  str s) {  str ret; append(ret, 'I'); append(ret, s); return ret; }
     str f_IR_124(rational p,  str s,  char b) {  return pushIR(s); }
     str f_ML_125(rational p,  char b,  str s) {  str ret; append(ret, 'M'); append(ret, s); return ret; }
     str f_D_126(rational p,  str s) {  return pushD(s); }
     str f_IL_127(rational p,  char b,  str s) {  str ret; append(ret, 'I'); append(ret, s); return ret; }
     str f_ML_128(rational p,  char b,  str s) {  str ret; append(ret, 'M'); append(ret, s); return ret; }
     str f_D_129(rational p,  str s) {  return pushD(s); }
     str f_IL_130(rational p,  char b,  str s) {  str ret; append(ret, 'I'); append(ret, s); return ret; }
     str f_ML_131(rational p,  char b,  str s) {  str ret; append(ret, 'M'); append(ret, s); return ret; }
     str f_D_132(rational p,  str s) {  return pushD(s); }
     str f_IL_133(rational p,  char b,  str s) {  str ret; append(ret, 'I'); append(ret, s); return ret; }
     str f_ML_134(rational p,  char b,  str s) {  str ret; append(ret, 'M'); append(ret, s); return ret; }
     str f_D_135(rational p,  str s) {  return pushD(s); }
     str f_IL_136(rational p,  char b,  str s) {  str ret; append(ret, 'I'); append(ret, s); return ret; }
     str f_ML_137(rational p,  char b,  str s) {  str ret; append(ret, 'M'); append(ret, s); return ret; }
     str f_D_138(rational p,  str s) {  return pushD(s); }
     str f_IL_139(rational p,  char b,  str s) {  str ret; append(ret, 'I'); append(ret, s); return ret; }
     str f_ML_140(rational p,  char b,  str s) {  str ret; append(ret, 'M'); append(ret, s); return ret; }
     str f_D_141(rational p,  str s) {  return pushD(s); }
     str f_E_143(rational p,  str s) {  str ret; return ret; }
     str nil(void) {  str ret; return ret; }
    choice [str] h([str] l) { return l; }
}

algebra Probability implements Algebra(alphabet = char, answer = rational) {  
rational     f_IL_1(rational p,  char b,  rational s) { return p * s * lookup(b, 'a','c','g','u', 1 $ 4,1 $ 4,1 $ 4,1 $ 4) ; }
rational     f_IR_2(rational p,  rational s,  char b) { return p * s * lookup(b, 'a','c','g','u', 1 $ 4,1 $ 4,1 $ 4,1 $ 4) ; }
rational     f_MP_3(rational p,  char a,  rational s,  char b) { return p * s * lookup2(a,b, 'a','c','g','u', 'a','c','g','u', 7732876500256921 $ 4611686018427387904,7732876500256921 $ 4611686018427387904,8837446636361073 $ 2305843009213693952,3390823426855141 $ 1125899906842624,7732876500256921 $ 4611686018427387904,1627627494141507 $ 36028797018963968,5819176307031577 $ 9007199254740992,1565233619000693 $ 144115188075855872,7732876500256921 $ 4611686018427387904,6592179737077133 $ 18014398509481984,7732876500256921 $ 4611686018427387904,1177397129198665 $ 18014398509481984,7732876500256921 $ 4611686018427387904,7732876500256921 $ 4611686018427387904,7732876500256921 $ 4611686018427387904,7732876500256921 $ 4611686018427387904) ; }
rational     f_ML_4(rational p,  char b,  rational s) { return p * s * lookup(b, 'a','c','g','u', 1 $ 4,1 $ 4,1 $ 4,1 $ 4) ; }
rational     f_MR_5(rational p,  rational s,  char b) { return p * s * lookup(b, 'a','c','g','u', 1 $ 4,1 $ 4,1 $ 4,1 $ 4) ; }
rational     f_D_6(rational p,  rational s) { return p * s ; }
rational     f_IL_7(rational p,  char b,  rational s) { return p * s * lookup(b, 'a','c','g','u', 1 $ 4,1 $ 4,1 $ 4,1 $ 4) ; }
rational     f_IR_8(rational p,  rational s,  char b) { return p * s * lookup(b, 'a','c','g','u', 1 $ 4,1 $ 4,1 $ 4,1 $ 4) ; }
rational     f_MP_9(rational p,  char a,  rational s,  char b) { return p * s * lookup2(a,b, 'a','c','g','u', 'a','c','g','u', 7732876500256921 $ 4611686018427387904,7732876500256921 $ 4611686018427387904,4542136932436187 $ 288230376151711744,7732876500256921 $ 4611686018427387904,2926030569711885 $ 18014398509481984,7732876500256921 $ 4611686018427387904,1231416704425107 $ 4503599627370496,7732876500256921 $ 4611686018427387904,5771228491895097 $ 288230376151711744,7732876500256921 $ 4611686018427387904,4542136932436187 $ 288230376151711744,7732876500256921 $ 4611686018427387904,3418067729343353 $ 562949953421312,7732876500256921 $ 4611686018427387904,7732876500256921 $ 4611686018427387904,7732876500256921 $ 4611686018427387904) ; }
rational     f_ML_10(rational p,  char b,  rational s) { return p * s * lookup(b, 'a','c','g','u', 1 $ 4,1 $ 4,1 $ 4,1 $ 4) ; }
rational     f_MR_11(rational p,  rational s,  char b) { return p * s * lookup(b, 'a','c','g','u', 1 $ 4,1 $ 4,1 $ 4,1 $ 4) ; }
rational     f_D_12(rational p,  rational s) { return p * s ; }
rational     f_IL_13(rational p,  char b,  rational s) { return p * s * lookup(b, 'a','c','g','u', 1 $ 4,1 $ 4,1 $ 4,1 $ 4) ; }
rational     f_IR_14(rational p,  rational s,  char b) { return p * s * lookup(b, 'a','c','g','u', 6818121758016165 $ 36028797018963968,8745302621980545 $ 18014398509481984,6818121758016165 $ 36028797018963968,6818121758016165 $ 36028797018963968) ; }
rational     f_MP_15(rational p,  char a,  rational s,  char b) { return p * s * lookup2(a,b, 'a','c','g','u', 'a','c','g','u', 7732876500256921 $ 4611686018427387904,7732876500256921 $ 4611686018427387904,7732876500256921 $ 4611686018427387904,624655531143665 $ 2251799813685248,7732876500256921 $ 4611686018427387904,6097281113954441 $ 72057594037927936,2847685874885601 $ 1125899906842624,4705873485884669 $ 144115188075855872,7732876500256921 $ 4611686018427387904,7732876500256921 $ 4611686018427387904,4849969561986603 $ 576460752303423488,7732876500256921 $ 4611686018427387904,2668322080813987 $ 4503599627370496,7732876500256921 $ 4611686018427387904,7003021345492457 $ 72057594037927936,7732876500256921 $ 4611686018427387904) ; }
rational     f_ML_16(rational p,  char b,  rational s) { return p * s * lookup(b, 'a','c','g','u', 1 $ 4,1 $ 4,1 $ 4,1 $ 4) ; }
rational     f_MR_17(rational p,  rational s,  char b) { return p * s * lookup(b, 'a','c','g','u', 1 $ 4,1 $ 4,1 $ 4,1 $ 4) ; }
rational     f_D_18(rational p,  rational s) { return p * s ; }
rational     f_IL_19(rational p,  char b,  rational s) { return p * s * lookup(b, 'a','c','g','u', 1 $ 4,1 $ 4,1 $ 4,1 $ 4) ; }
rational     f_IR_20(rational p,  rational s,  char b) { return p * s * lookup(b, 'a','c','g','u', 1 $ 4,1 $ 4,1 $ 4,1 $ 4) ; }
rational     f_MP_21(rational p,  char a,  rational s,  char b) { return p * s * lookup2(a,b, 'a','c','g','u', 'a','c','g','u', 3922623479264429 $ 2305843009213693952,3922623479264429 $ 2305843009213693952,420910362014599 $ 18014398509481984,3922623479264429 $ 2305843009213693952,2805755998365081 $ 72057594037927936,7834998963829483 $ 1152921504606846976,6382927063428339 $ 2251799813685248,3922623479264429 $ 2305843009213693952,3922623479264429 $ 2305843009213693952,3922623479264429 $ 2305843009213693952,3922623479264429 $ 2305843009213693952,3922623479264429 $ 2305843009213693952,5604941686660367 $ 9007199254740992,6293055164513087 $ 288230376151711744,1902312149353507 $ 9007199254740992,6053456780725219 $ 72057594037927936) ; }
rational     f_ML_22(rational p,  char b,  rational s) { return p * s * lookup(b, 'a','c','g','u', 1 $ 4,1 $ 4,1 $ 4,1 $ 4) ; }
rational     f_MR_23(rational p,  rational s,  char b) { return p * s * lookup(b, 'a','c','g','u', 835224072043923 $ 4503599627370496,835224072043923 $ 4503599627370496,4539784770354387 $ 9007199254740992,835224072043923 $ 4503599627370496) ; }
rational     f_D_24(rational p,  rational s) { return p * s ; }
rational     f_IL_25(rational p,  char b,  rational s) { return p * s * lookup(b, 'a','c','g','u', 1 $ 4,1 $ 4,1 $ 4,1 $ 4) ; }
rational     f_IR_26(rational p,  rational s,  char b) { return p * s * lookup(b, 'a','c','g','u', 1 $ 4,1 $ 4,1 $ 4,1 $ 4) ; }
rational     f_MP_27(rational p,  char a,  rational s,  char b) { return p * s * lookup2(a,b, 'a','c','g','u', 'a','c','g','u', 2376819123634799 $ 72057594037927936,4742397783640311 $ 1152921504606846976,7611127106904531 $ 4611686018427387904,4527932166018077 $ 2251799813685248,2723436256829387 $ 576460752303423488,4742397783640311 $ 1152921504606846976,2820915242506363 $ 2251799813685248,710045505274511 $ 36028797018963968,7611127106904531 $ 4611686018427387904,4742397783640311 $ 1152921504606846976,7611127106904531 $ 4611686018427387904,1540589976552535 $ 144115188075855872,614820713276287 $ 2251799813685248,4742397783640311 $ 1152921504606846976,2723436256829387 $ 576460752303423488,4763613541576581 $ 288230376151711744) ; }
rational     f_ML_28(rational p,  char b,  rational s) { return p * s * lookup(b, 'a','c','g','u', 1 $ 4,1 $ 4,1 $ 4,1 $ 4) ; }
rational     f_MR_29(rational p,  rational s,  char b) { return p * s * lookup(b, 'a','c','g','u', 1 $ 4,1 $ 4,1 $ 4,1 $ 4) ; }
rational     f_D_30(rational p,  rational s) { return p * s ; }
rational     f_IL_31(rational p,  char b,  rational s) { return p * s * lookup(b, 'a','c','g','u', 1 $ 4,1 $ 4,1 $ 4,1 $ 4) ; }
rational     f_IR_32(rational p,  rational s,  char b) { return p * s * lookup(b, 'a','c','g','u', 1 $ 4,1 $ 4,1 $ 4,1 $ 4) ; }
rational     f_MP_33(rational p,  char a,  rational s,  char b) { return p * s * lookup2(a,b, 'a','c','g','u', 'a','c','g','u', 7732876500256921 $ 4611686018427387904,6172930269050955 $ 36028797018963968,7732876500256921 $ 4611686018427387904,2801711066603375 $ 72057594037927936,7732876500256921 $ 4611686018427387904,7732876500256921 $ 4611686018427387904,5819176307031577 $ 9007199254740992,7732876500256921 $ 4611686018427387904,876403816722919 $ 144115188075855872,7523970677070805 $ 2251799813685248,7722775291486249 $ 1152921504606846976,2342775038653893 $ 72057594037927936,1211700211735111 $ 72057594037927936,5880489481485971 $ 288230376151711744,7732876500256921 $ 4611686018427387904,7732876500256921 $ 4611686018427387904) ; }
rational     f_ML_34(rational p,  char b,  rational s) { return p * s * lookup(b, 'a','c','g','u', 1 $ 4,1 $ 4,1 $ 4,1 $ 4) ; }
rational     f_MR_35(rational p,  rational s,  char b) { return p * s * lookup(b, 'a','c','g','u', 1 $ 4,1 $ 4,1 $ 4,1 $ 4) ; }
rational     f_D_36(rational p,  rational s) { return p * s ; }
rational     f_IL_37(rational p,  char b,  rational s) { return p * s * lookup(b, 'a','c','g','u', 1 $ 4,1 $ 4,1 $ 4,1 $ 4) ; }
rational     f_IR_38(rational p,  rational s,  char b) { return p * s * lookup(b, 'a','c','g','u', 7765553472452089 $ 2251799813685248,6156664160984663 $ 1152921504606846976,6156664160984663 $ 1152921504606846976,6156664160984663 $ 1152921504606846976) ; }
rational     f_ML_39(rational p,  char b,  rational s) { return p * s * lookup(b, 'a','c','g','u', 2544890811079023 $ 144115188075855872,3832687659936309 $ 1125899906842624,1233164572976935 $ 2305843009213693952,6484872261412641 $ 1152921504606846976) ; }
rational     f_D_40(rational p,  rational s) { return p * s ; }
rational     f_IL_41(rational p,  char b,  rational s) { return p * s * lookup(b, 'a','c','g','u', 1 $ 4,1 $ 4,1 $ 4,1 $ 4) ; }
rational     f_ML_42(rational p,  char b,  rational s) { return p * s * lookup(b, 'a','c','g','u', 4948253007200799 $ 576460752303423488,666283266335815 $ 36028797018963968,4635954742579995 $ 9223372036854775808,934957803330839 $ 281474976710656) ; }
rational     f_D_43(rational p,  rational s) { return p * s ; }
rational     f_IL_44(rational p,  char b,  rational s) { return p * s * lookup(b, 'a','c','g','u', 1 $ 4,1 $ 4,1 $ 4,1 $ 4) ; }
rational     f_ML_45(rational p,  char b,  rational s) { return p * s * lookup(b, 'a','c','g','u', 4635954742579995 $ 9223372036854775808,2314949471729943 $ 1152921504606846976,4530892412835625 $ 1125899906842624,4635954742579995 $ 9223372036854775808) ; }
rational     f_D_46(rational p,  rational s) { return p * s ; }
rational     f_IL_47(rational p,  char b,  rational s) { return p * s * lookup(b, 'a','c','g','u', 1 $ 4,1 $ 4,1 $ 4,1 $ 4) ; }
rational     f_ML_48(rational p,  char b,  rational s) { return p * s * lookup(b, 'a','c','g','u', 2083596376411117 $ 562949953421312,4635954742579995 $ 9223372036854775808,8523227159295315 $ 2305843009213693952,6919845182222927 $ 1152921504606846976) ; }
rational     f_D_49(rational p,  rational s) { return p * s ; }
rational     f_IL_50(rational p,  char b,  rational s) { return p * s * lookup(b, 'a','c','g','u', 1 $ 4,1 $ 4,1 $ 4,1 $ 4) ; }
rational     f_ML_51(rational p,  char b,  rational s) { return p * s * lookup(b, 'a','c','g','u', 5805416269723485 $ 18014398509481984,6721374381233391 $ 9007199254740992,1792566557692803 $ 36028797018963968,4532625423517965 $ 36028797018963968) ; }
rational     f_D_52(rational p,  rational s) { return p * s ; }
rational     f_IL_53(rational p,  char b,  rational s) { return p * s * lookup(b, 'a','c','g','u', 1 $ 4,1 $ 4,1 $ 4,1 $ 4) ; }
rational     f_ML_54(rational p,  char b,  rational s) { return p * s * lookup(b, 'a','c','g','u', 6919845182222927 $ 1152921504606846976,6246129856340495 $ 1152921504606846976,513438058111537 $ 140737488355328,4635954742579995 $ 9223372036854775808) ; }
rational     f_D_55(rational p,  rational s) { return p * s ; }
rational     f_IL_56(rational p,  char b,  rational s) { return p * s * lookup(b, 'a','c','g','u', 1 $ 4,1 $ 4,1 $ 4,1 $ 4) ; }
rational     f_ML_57(rational p,  char b,  rational s) { return p * s * lookup(b, 'a','c','g','u', 4191310227490359 $ 1125899906842624,8950515262917027 $ 9223372036854775808,702720371627531 $ 144115188075855872,6854792841609001 $ 2305843009213693952) ; }
rational     f_D_58(rational p,  rational s) { return p * s ; }
rational     f_IL_59(rational p,  char b,  rational s) { return p * s * lookup(b, 'a','c','g','u', 1 $ 4,1 $ 4,1 $ 4,1 $ 4) ; }
rational     f_MR_60(rational p,  rational s,  char b) { return p * s * lookup(b, 'a','c','g','u', 901470485331727 $ 18014398509481984,6041599500117977 $ 2251799813685248,1708992327213019 $ 144115188075855872,4014223004549081 $ 576460752303423488) ; }
rational     f_D_61(rational p,  rational s) { return p * s ; }
rational     f_IR_62(rational p,  rational s,  char b) { return p * s * lookup(b, 'a','c','g','u', 1 $ 4,1 $ 4,1 $ 4,1 $ 4) ; }
rational     f_B_63(rational s,  rational t) { return s * t ; }
rational     f_S_64(rational p,  rational s) { return p * s ; }
rational     f_IL_65(rational p,  char b,  rational s) { return p * s * lookup(b, 'a','c','g','u', 1 $ 4,1 $ 4,1 $ 4,1 $ 4) ; }
rational     f_ML_66(rational p,  char b,  rational s) { return p * s * lookup(b, 'a','c','g','u', 4635954742579995 $ 9223372036854775808,4635954742579995 $ 9223372036854775808,7732016124599523 $ 2251799813685248,4718831584394091 $ 144115188075855872) ; }
rational     f_D_67(rational p,  rational s) { return p * s ; }
rational     f_IL_68(rational p,  char b,  rational s) { return p * s * lookup(b, 'a','c','g','u', 1 $ 4,1 $ 4,1 $ 4,1 $ 4) ; }
rational     f_ML_69(rational p,  char b,  rational s) { return p * s * lookup(b, 'a','c','g','u', 2175756192903719 $ 562949953421312,4635954742579995 $ 9223372036854775808,3852256999881401 $ 1152921504606846976,2268661772818551 $ 1152921504606846976) ; }
rational     f_D_70(rational p,  rational s) { return p * s ; }
rational     f_IL_71(rational p,  char b,  rational s) { return p * s * lookup(b, 'a','c','g','u', 1 $ 4,1 $ 4,1 $ 4,1 $ 4) ; }
rational     f_ML_72(rational p,  char b,  rational s) { return p * s * lookup(b, 'a','c','g','u', 4276825886183401 $ 1125899906842624,4876054992189021 $ 9223372036854775808,4765435565231505 $ 2305843009213693952,2859968099964195 $ 576460752303423488) ; }
rational     f_D_73(rational p,  rational s) { return p * s ; }
rational     f_IL_74(rational p,  char b,  rational s) { return p * s * lookup(b, 'a','c','g','u', 5424084239000897 $ 72057594037927936,5424084239000897 $ 72057594037927936,1762277893462601 $ 2251799813685248,5983992080716333 $ 18014398509481984) ; }
rational     f_MP_75(rational p,  char a,  rational s,  char b) { return p * s * lookup2(a,b, 'a','c','g','u', 'a','c','g','u', 5541991616650031 $ 1152921504606846976,7732876500256921 $ 4611686018427387904,7732876500256921 $ 4611686018427387904,3551516179611361 $ 281474976710656,7732876500256921 $ 4611686018427387904,7732876500256921 $ 4611686018427387904,7732876500256921 $ 4611686018427387904,7732876500256921 $ 4611686018427387904,7732876500256921 $ 4611686018427387904,7732876500256921 $ 4611686018427387904,7732876500256921 $ 4611686018427387904,6580891462514541 $ 288230376151711744,7732876500256921 $ 4611686018427387904,7732876500256921 $ 4611686018427387904,7732876500256921 $ 4611686018427387904,7732876500256921 $ 4611686018427387904) ; }
rational     f_ML_76(rational p,  char b,  rational s) { return p * s * lookup(b, 'a','c','g','u', 1 $ 4,1 $ 4,1 $ 4,1 $ 4) ; }
rational     f_MR_77(rational p,  rational s,  char b) { return p * s * lookup(b, 'a','c','g','u', 1 $ 4,1 $ 4,1 $ 4,1 $ 4) ; }
rational     f_D_78(rational p,  rational s) { return p * s ; }
rational     f_IL_79(rational p,  char b,  rational s) { return p * s * lookup(b, 'a','c','g','u', 1 $ 4,1 $ 4,1 $ 4,1 $ 4) ; }
rational     f_IR_80(rational p,  rational s,  char b) { return p * s * lookup(b, 'a','c','g','u', 1 $ 4,1 $ 4,1 $ 4,1 $ 4) ; }
rational     f_MP_81(rational p,  char a,  rational s,  char b) { return p * s * lookup2(a,b, 'a','c','g','u', 'a','c','g','u', 7732876500256921 $ 4611686018427387904,7732876500256921 $ 4611686018427387904,7732876500256921 $ 4611686018427387904,7732876500256921 $ 4611686018427387904,6818121758016165 $ 144115188075855872,7732876500256921 $ 4611686018427387904,6406424427700241 $ 1125899906842624,372921925947957 $ 4503599627370496,7732876500256921 $ 4611686018427387904,5911459740931657 $ 36028797018963968,6425651086221555 $ 576460752303423488,7732876500256921 $ 4611686018427387904,3498274296909733 $ 288230376151711744,7732876500256921 $ 4611686018427387904,3169736743852029 $ 36028797018963968,7732876500256921 $ 4611686018427387904) ; }
rational     f_ML_82(rational p,  char b,  rational s) { return p * s * lookup(b, 'a','c','g','u', 1 $ 4,1 $ 4,1 $ 4,1 $ 4) ; }
rational     f_MR_83(rational p,  rational s,  char b) { return p * s * lookup(b, 'a','c','g','u', 1 $ 4,1 $ 4,1 $ 4,1 $ 4) ; }
rational     f_D_84(rational p,  rational s) { return p * s ; }
rational     f_IL_85(rational p,  char b,  rational s) { return p * s * lookup(b, 'a','c','g','u', 1 $ 4,1 $ 4,1 $ 4,1 $ 4) ; }
rational     f_IR_86(rational p,  rational s,  char b) { return p * s * lookup(b, 'a','c','g','u', 1 $ 4,1 $ 4,1 $ 4,1 $ 4) ; }
rational     f_MP_87(rational p,  char a,  rational s,  char b) { return p * s * lookup2(a,b, 'a','c','g','u', 'a','c','g','u', 7902043055276565 $ 4611686018427387904,6000469608022181 $ 288230376151711744,7902043055276565 $ 4611686018427387904,2858874615547267 $ 72057594037927936,7902043055276565 $ 4611686018427387904,3966104018979571 $ 288230376151711744,7902043055276565 $ 4611686018427387904,7902043055276565 $ 4611686018427387904,5307246137326645 $ 1152921504606846976,1661511748058851 $ 562949953421312,6773541843865707 $ 288230376151711744,4945690536436967 $ 288230376151711744,743309778109107 $ 1125899906842624,1201256793628301 $ 72057594037927936,7902043055276565 $ 4611686018427387904,3016035878151387 $ 18014398509481984) ; }
rational     f_ML_88(rational p,  char b,  rational s) { return p * s * lookup(b, 'a','c','g','u', 1 $ 4,1 $ 4,1 $ 4,1 $ 4) ; }
rational     f_MR_89(rational p,  rational s,  char b) { return p * s * lookup(b, 'a','c','g','u', 2942565903886781 $ 18014398509481984,5644753641155601 $ 9007199254740992,2942565903886781 $ 18014398509481984,2942565903886781 $ 18014398509481984) ; }
rational     f_D_90(rational p,  rational s) { return p * s ; }
rational     f_IL_91(rational p,  char b,  rational s) { return p * s * lookup(b, 'a','c','g','u', 1 $ 4,1 $ 4,1 $ 4,1 $ 4) ; }
rational     f_IR_92(rational p,  rational s,  char b) { return p * s * lookup(b, 'a','c','g','u', 1 $ 4,1 $ 4,1 $ 4,1 $ 4) ; }
rational     f_MP_93(rational p,  char a,  rational s,  char b) { return p * s * lookup2(a,b, 'a','c','g','u', 'a','c','g','u', 7902043055276565 $ 4611686018427387904,2320750991036781 $ 144115188075855872,664363550104441 $ 144115188075855872,7902043055276565 $ 4611686018427387904,1170622196326475 $ 18014398509481984,7902043055276565 $ 4611686018427387904,8263673248056531 $ 2251799813685248,5296850505709623 $ 144115188075855872,2320750991036781 $ 144115188075855872,2418981485780817 $ 4503599627370496,4811964366312075 $ 288230376151711744,7902043055276565 $ 4611686018427387904,7902043055276565 $ 4611686018427387904,7902043055276565 $ 4611686018427387904,6813667155832575 $ 72057594037927936,7902043055276565 $ 4611686018427387904) ; }
rational     f_ML_94(rational p,  char b,  rational s) { return p * s * lookup(b, 'a','c','g','u', 1 $ 4,1 $ 4,1 $ 4,1 $ 4) ; }
rational     f_MR_95(rational p,  rational s,  char b) { return p * s * lookup(b, 'a','c','g','u', 1 $ 4,1 $ 4,1 $ 4,1 $ 4) ; }
rational     f_D_96(rational p,  rational s) { return p * s ; }
rational     f_E_99(rational p,  rational s) { return p * s ; }
rational     f_S_100(rational p,  rational s) { return p * s ; }
rational     f_MP_101(rational p,  char a,  rational s,  char b) { return p * s * lookup2(a,b, 'a','c','g','u', 'a','c','g','u', 7732876500256921 $ 4611686018427387904,5771228491895097 $ 288230376151711744,7732876500256921 $ 4611686018427387904,7732876500256921 $ 4611686018427387904,7732876500256921 $ 4611686018427387904,7732876500256921 $ 4611686018427387904,7732876500256921 $ 4611686018427387904,7732876500256921 $ 4611686018427387904,7732876500256921 $ 4611686018427387904,3012912956735549 $ 281474976710656,8260312934212067 $ 144115188075855872,578516092634577 $ 36028797018963968,7732876500256921 $ 4611686018427387904,7732876500256921 $ 4611686018427387904,7732876500256921 $ 4611686018427387904,7732876500256921 $ 4611686018427387904) ; }
rational     f_ML_102(rational p,  char b,  rational s) { return p * s * lookup(b, 'a','c','g','u', 1 $ 4,1 $ 4,1 $ 4,1 $ 4) ; }
rational     f_MR_103(rational p,  rational s,  char b) { return p * s * lookup(b, 'a','c','g','u', 1 $ 4,1 $ 4,1 $ 4,1 $ 4) ; }
rational     f_D_104(rational p,  rational s) { return p * s ; }
rational     f_IL_105(rational p,  char b,  rational s) { return p * s * lookup(b, 'a','c','g','u', 1 $ 4,1 $ 4,1 $ 4,1 $ 4) ; }
rational     f_IR_106(rational p,  rational s,  char b) { return p * s * lookup(b, 'a','c','g','u', 1 $ 4,1 $ 4,1 $ 4,1 $ 4) ; }
rational     f_MP_107(rational p,  char a,  rational s,  char b) { return p * s * lookup2(a,b, 'a','c','g','u', 'a','c','g','u', 7811365435831113 $ 4611686018427387904,7811365435831113 $ 4611686018427387904,7811365435831113 $ 4611686018427387904,5878241126473901 $ 9007199254740992,8927146992969469 $ 2305843009213693952,7811365435831113 $ 4611686018427387904,7811365435831113 $ 4611686018427387904,7811365435831113 $ 4611686018427387904,7811365435831113 $ 4611686018427387904,2981426226991587 $ 18014398509481984,7811365435831113 $ 4611686018427387904,7811365435831113 $ 4611686018427387904,6021072604461847 $ 1125899906842624,7811365435831113 $ 4611686018427387904,3465857433890173 $ 144115188075855872,7811365435831113 $ 4611686018427387904) ; }
rational     f_ML_108(rational p,  char b,  rational s) { return p * s * lookup(b, 'a','c','g','u', 7296470213346989 $ 36028797018963968,7296470213346989 $ 36028797018963968,7296470213346989 $ 36028797018963968,7625216260552425 $ 18014398509481984) ; }
rational     f_MR_109(rational p,  rational s,  char b) { return p * s * lookup(b, 'a','c','g','u', 1 $ 4,1 $ 4,1 $ 4,1 $ 4) ; }
rational     f_D_110(rational p,  rational s) { return p * s ; }
rational     f_IL_111(rational p,  char b,  rational s) { return p * s * lookup(b, 'a','c','g','u', 1 $ 4,1 $ 4,1 $ 4,1 $ 4) ; }
rational     f_IR_112(rational p,  rational s,  char b) { return p * s * lookup(b, 'a','c','g','u', 1 $ 4,1 $ 4,1 $ 4,1 $ 4) ; }
rational     f_MP_113(rational p,  char a,  rational s,  char b) { return p * s * lookup2(a,b, 'a','c','g','u', 'a','c','g','u', 7732876500256921 $ 4611686018427387904,7732876500256921 $ 4611686018427387904,7732876500256921 $ 4611686018427387904,7732876500256921 $ 4611686018427387904,7344491768865533 $ 72057594037927936,1664095683449217 $ 288230376151711744,98524362623955 $ 17592186044416,587770173184481 $ 36028797018963968,7732876500256921 $ 4611686018427387904,7732876500256921 $ 4611686018427387904,2329294333437357 $ 72057594037927936,7732876500256921 $ 4611686018427387904,3929884748735551 $ 36028797018963968,7732876500256921 $ 4611686018427387904,3020390238407661 $ 18014398509481984,7732876500256921 $ 4611686018427387904) ; }
rational     f_ML_114(rational p,  char b,  rational s) { return p * s * lookup(b, 'a','c','g','u', 1 $ 4,1 $ 4,1 $ 4,1 $ 4) ; }
rational     f_MR_115(rational p,  rational s,  char b) { return p * s * lookup(b, 'a','c','g','u', 1 $ 4,1 $ 4,1 $ 4,1 $ 4) ; }
rational     f_D_116(rational p,  rational s) { return p * s ; }
rational     f_IL_117(rational p,  char b,  rational s) { return p * s * lookup(b, 'a','c','g','u', 1 $ 4,1 $ 4,1 $ 4,1 $ 4) ; }
rational     f_IR_118(rational p,  rational s,  char b) { return p * s * lookup(b, 'a','c','g','u', 1 $ 4,1 $ 4,1 $ 4,1 $ 4) ; }
rational     f_MP_119(rational p,  char a,  rational s,  char b) { return p * s * lookup2(a,b, 'a','c','g','u', 'a','c','g','u', 7732876500256921 $ 4611686018427387904,7732876500256921 $ 4611686018427387904,7732876500256921 $ 4611686018427387904,7732876500256921 $ 4611686018427387904,2827916034810773 $ 144115188075855872,7761343411156979 $ 2305843009213693952,3301743687269807 $ 562949953421312,7732876500256921 $ 4611686018427387904,5819176307031577 $ 9007199254740992,7732876500256921 $ 4611686018427387904,5209342354016211 $ 288230376151711744,7732876500256921 $ 4611686018427387904,7732876500256921 $ 4611686018427387904,7732876500256921 $ 4611686018427387904,3915470790601843 $ 72057594037927936,7732876500256921 $ 4611686018427387904) ; }
rational     f_ML_120(rational p,  char b,  rational s) { return p * s * lookup(b, 'a','c','g','u', 1 $ 4,1 $ 4,1 $ 4,1 $ 4) ; }
rational     f_MR_121(rational p,  rational s,  char b) { return p * s * lookup(b, 'a','c','g','u', 1 $ 4,1 $ 4,1 $ 4,1 $ 4) ; }
rational     f_D_122(rational p,  rational s) { return p * s ; }
rational     f_IL_123(rational p,  char b,  rational s) { return p * s * lookup(b, 'a','c','g','u', 1 $ 4,1 $ 4,1 $ 4,1 $ 4) ; }
rational     f_IR_124(rational p,  rational s,  char b) { return p * s * lookup(b, 'a','c','g','u', 1 $ 4,1 $ 4,1 $ 4,1 $ 4) ; }
rational     f_ML_125(rational p,  char b,  rational s) { return p * s * lookup(b, 'a','c','g','u', 8160189167938583 $ 18014398509481984,6010762278677161 $ 4503599627370496,1659076097344825 $ 288230376151711744,294269481385849 $ 18014398509481984) ; }
rational     f_D_126(rational p,  rational s) { return p * s ; }
rational     f_IL_127(rational p,  char b,  rational s) { return p * s * lookup(b, 'a','c','g','u', 1 $ 4,1 $ 4,1 $ 4,1 $ 4) ; }
rational     f_ML_128(rational p,  char b,  rational s) { return p * s * lookup(b, 'a','c','g','u', 778430044545447 $ 281474976710656,662269768642603 $ 576460752303423488,4297019396691121 $ 36028797018963968,4635954742579995 $ 9223372036854775808) ; }
rational     f_D_129(rational p,  rational s) { return p * s ; }
rational     f_IL_130(rational p,  char b,  rational s) { return p * s * lookup(b, 'a','c','g','u', 7239293817774307 $ 18014398509481984,1608950123557685 $ 9007199254740992,7155251260457795 $ 72057594037927936,7176899694702559 $ 18014398509481984) ; }
rational     f_ML_131(rational p,  char b,  rational s) { return p * s * lookup(b, 'a','c','g','u', 7517016422416291 $ 4503599627370496,6977335794150049 $ 36028797018963968,5696777776890297 $ 72057594037927936,4635954742579995 $ 9223372036854775808) ; }
rational     f_D_132(rational p,  rational s) { return p * s ; }
rational     f_IL_133(rational p,  char b,  rational s) { return p * s * lookup(b, 'a','c','g','u', 1 $ 4,1 $ 4,1 $ 4,1 $ 4) ; }
rational     f_ML_134(rational p,  char b,  rational s) { return p * s * lookup(b, 'a','c','g','u', 6456147428976877 $ 2251799813685248,3525425900631015 $ 576460752303423488,655881959304679 $ 9007199254740992,4635954742579995 $ 9223372036854775808) ; }
rational     f_D_135(rational p,  rational s) { return p * s ; }
rational     f_IL_136(rational p,  char b,  rational s) { return p * s * lookup(b, 'a','c','g','u', 1 $ 4,1 $ 4,1 $ 4,1 $ 4) ; }
rational     f_ML_137(rational p,  char b,  rational s) { return p * s * lookup(b, 'a','c','g','u', 38305191814233 $ 562949953421312,435170301988561 $ 1125899906842624,293957638555449 $ 576460752303423488,5706549521650699 $ 4503599627370496) ; }
rational     f_D_138(rational p,  rational s) { return p * s ; }
rational     f_IL_139(rational p,  char b,  rational s) { return p * s * lookup(b, 'a','c','g','u', 1 $ 4,1 $ 4,1 $ 4,1 $ 4) ; }
rational     f_ML_140(rational p,  char b,  rational s) { return p * s * lookup(b, 'a','c','g','u', 477705600014537 $ 140737488355328,4635954742579995 $ 9223372036854775808,894285158260607 $ 36028797018963968,6894464702863987 $ 2305843009213693952) ; }
rational     f_D_141(rational p,  rational s) { return p * s ; }
rational     f_E_143(rational p,  rational s) { return p * s ; }
rational     nil(void) { return 1 $ 1 ; }
choice [rational] h([rational] xs) { return list(sum(xs)); }

}



algebra ambuni extends Ambi { choice [str] h([str] l) { return unique(l); } }


algebra propmax extends Probability { choice [rational] h([rational] xs) { return list(maximum(xs)); } }


grammar Grammar uses Algebra (axiom = s_0) {
  s_0 = f_IL_1(CONST_RATIO(3931825344300771$36893488147419103232), CHAR, il_1) |
    f_IR_2(CONST_RATIO(3931825344300771$36893488147419103232), ir_2, CHAR) |
    f_MP_3(CONST_RATIO(985954264173203$1125899906842624), CHAR, mp_3, CHAR) |
    f_ML_4(CONST_RATIO(3931825344300771$36893488147419103232), CHAR, ml_4) |
    f_MR_5(CONST_RATIO(3931825344300771$36893488147419103232), mr_5, CHAR) |
    f_D_6(CONST_RATIO(3931825344300771$36893488147419103232), d_6) # h 
;


  il_1 = f_IL_1(CONST_RATIO(6919845182222927$288230376151711744), CHAR, il_1) |
    f_IR_2(CONST_RATIO(6919845182222927$288230376151711744), ir_2, CHAR) |
    f_MP_3(CONST_RATIO(6919845182222927$288230376151711744), CHAR, mp_3, CHAR) |
    f_ML_4(CONST_RATIO(6919845182222927$288230376151711744), CHAR, ml_4) |
    f_MR_5(CONST_RATIO(6919845182222927$288230376151711744), mr_5, CHAR) |
    f_D_6(CONST_RATIO(6919845182222927$288230376151711744), d_6) # h 
;


  ir_2 = f_IR_2(CONST_RATIO(5056502364935393$144115188075855872), ir_2, CHAR) |
    f_MP_3(CONST_RATIO(5056502364935393$144115188075855872), CHAR, mp_3, CHAR) |
    f_ML_4(CONST_RATIO(5056502364935393$144115188075855872), CHAR, ml_4) |
    f_MR_5(CONST_RATIO(5056502364935393$144115188075855872), mr_5, CHAR) |
    f_D_6(CONST_RATIO(5056502364935393$144115188075855872), d_6) # h 
;


  mp_3 = f_IL_7(CONST_RATIO(3931825344300771$36893488147419103232), CHAR, il_7) |
    f_IR_8(CONST_RATIO(3931825344300771$36893488147419103232), ir_8, CHAR) |
    f_MP_9(CONST_RATIO(985954264173203$1125899906842624), CHAR, mp_9, CHAR) |
    f_ML_10(CONST_RATIO(3931825344300771$36893488147419103232), CHAR, ml_10) |
    f_MR_11(CONST_RATIO(3931825344300771$36893488147419103232), mr_11, CHAR) |
    f_D_12(CONST_RATIO(3931825344300771$36893488147419103232), d_12) # h 
;


  ml_4 = f_IL_7(CONST_RATIO(6919845182222927$288230376151711744), CHAR, il_7) |
    f_IR_8(CONST_RATIO(6919845182222927$288230376151711744), ir_8, CHAR) |
    f_MP_9(CONST_RATIO(6919845182222927$288230376151711744), CHAR, mp_9, CHAR) |
    f_ML_10(CONST_RATIO(6919845182222927$288230376151711744), CHAR, ml_10) |
    f_MR_11(CONST_RATIO(6919845182222927$288230376151711744), mr_11, CHAR) |
    f_D_12(CONST_RATIO(6919845182222927$288230376151711744), d_12) # h 
;


  mr_5 = f_IL_7(CONST_RATIO(6919845182222927$288230376151711744), CHAR, il_7) |
    f_IR_8(CONST_RATIO(6919845182222927$288230376151711744), ir_8, CHAR) |
    f_MP_9(CONST_RATIO(6919845182222927$288230376151711744), CHAR, mp_9, CHAR) |
    f_ML_10(CONST_RATIO(6919845182222927$288230376151711744), CHAR, ml_10) |
    f_MR_11(CONST_RATIO(6919845182222927$288230376151711744), mr_11, CHAR) |
    f_D_12(CONST_RATIO(6919845182222927$288230376151711744), d_12) # h 
;


  d_6 = f_IL_7(CONST_RATIO(6919845182222927$288230376151711744), CHAR, il_7) |
    f_IR_8(CONST_RATIO(6919845182222927$288230376151711744), ir_8, CHAR) |
    f_MP_9(CONST_RATIO(6919845182222927$288230376151711744), CHAR, mp_9, CHAR) |
    f_ML_10(CONST_RATIO(6919845182222927$288230376151711744), CHAR, ml_10) |
    f_MR_11(CONST_RATIO(6919845182222927$288230376151711744), mr_11, CHAR) |
    f_D_12(CONST_RATIO(6919845182222927$288230376151711744), d_12) # h 
;


  il_7 = f_IL_7(CONST_RATIO(6919845182222927$288230376151711744), CHAR, il_7) |
    f_IR_8(CONST_RATIO(6919845182222927$288230376151711744), ir_8, CHAR) |
    f_MP_9(CONST_RATIO(6919845182222927$288230376151711744), CHAR, mp_9, CHAR) |
    f_ML_10(CONST_RATIO(6919845182222927$288230376151711744), CHAR, ml_10) |
    f_MR_11(CONST_RATIO(6919845182222927$288230376151711744), mr_11, CHAR) |
    f_D_12(CONST_RATIO(6919845182222927$288230376151711744), d_12) # h 
;


  ir_8 = f_IR_8(CONST_RATIO(5056502364935393$144115188075855872), ir_8, CHAR) |
    f_MP_9(CONST_RATIO(5056502364935393$144115188075855872), CHAR, mp_9, CHAR) |
    f_ML_10(CONST_RATIO(5056502364935393$144115188075855872), CHAR, ml_10) |
    f_MR_11(CONST_RATIO(5056502364935393$144115188075855872), mr_11, CHAR) |
    f_D_12(CONST_RATIO(5056502364935393$144115188075855872), d_12) # h 
;


  mp_9 = f_IL_13(CONST_RATIO(3931825344300771$36893488147419103232), CHAR, il_13) |
    f_IR_14(CONST_RATIO(5043178123396837$18446744073709551616), ir_14, CHAR) |
    f_MP_15(CONST_RATIO(242607754888089$281474976710656), CHAR, mp_15, CHAR) |
    f_ML_16(CONST_RATIO(3931825344300771$36893488147419103232), CHAR, ml_16) |
    f_MR_17(CONST_RATIO(3931825344300771$36893488147419103232), mr_17, CHAR) |
    f_D_18(CONST_RATIO(3931825344300771$36893488147419103232), d_18) # h 
;


  ml_10 = f_IL_13(CONST_RATIO(6919845182222927$288230376151711744), CHAR, il_13) |
    f_IR_14(CONST_RATIO(6919845182222927$288230376151711744), ir_14, CHAR) |
    f_MP_15(CONST_RATIO(6919845182222927$288230376151711744), CHAR, mp_15, CHAR) |
    f_ML_16(CONST_RATIO(6919845182222927$288230376151711744), CHAR, ml_16) |
    f_MR_17(CONST_RATIO(6919845182222927$288230376151711744), mr_17, CHAR) |
    f_D_18(CONST_RATIO(6919845182222927$288230376151711744), d_18) # h 
;


  mr_11 = f_IL_13(CONST_RATIO(6919845182222927$288230376151711744), CHAR, il_13) |
    f_IR_14(CONST_RATIO(6919845182222927$288230376151711744), ir_14, CHAR) |
    f_MP_15(CONST_RATIO(6919845182222927$288230376151711744), CHAR, mp_15, CHAR) |
    f_ML_16(CONST_RATIO(6919845182222927$288230376151711744), CHAR, ml_16) |
    f_MR_17(CONST_RATIO(6919845182222927$288230376151711744), mr_17, CHAR) |
    f_D_18(CONST_RATIO(6919845182222927$288230376151711744), d_18) # h 
;


  d_12 = f_IL_13(CONST_RATIO(6919845182222927$288230376151711744), CHAR, il_13) |
    f_IR_14(CONST_RATIO(6919845182222927$288230376151711744), ir_14, CHAR) |
    f_MP_15(CONST_RATIO(6919845182222927$288230376151711744), CHAR, mp_15, CHAR) |
    f_ML_16(CONST_RATIO(6919845182222927$288230376151711744), CHAR, ml_16) |
    f_MR_17(CONST_RATIO(6919845182222927$288230376151711744), mr_17, CHAR) |
    f_D_18(CONST_RATIO(6919845182222927$288230376151711744), d_18) # h 
;


  il_13 = f_IL_13(CONST_RATIO(6919845182222927$288230376151711744), CHAR, il_13) |
    f_IR_14(CONST_RATIO(6919845182222927$288230376151711744), ir_14, CHAR) |
    f_MP_15(CONST_RATIO(6919845182222927$288230376151711744), CHAR, mp_15, CHAR) |
    f_ML_16(CONST_RATIO(6919845182222927$288230376151711744), CHAR, ml_16) |
    f_MR_17(CONST_RATIO(6919845182222927$288230376151711744), mr_17, CHAR) |
    f_D_18(CONST_RATIO(6919845182222927$288230376151711744), d_18) # h 
;


  ir_14 = f_IR_14(CONST_RATIO(8074910344859373$288230376151711744), ir_14, CHAR) |
    f_MP_15(CONST_RATIO(1292799743868447$18014398509481984), CHAR, mp_15, CHAR) |
    f_ML_16(CONST_RATIO(8074910344859373$288230376151711744), CHAR, ml_16) |
    f_MR_17(CONST_RATIO(8074910344859373$288230376151711744), mr_17, CHAR) |
    f_D_18(CONST_RATIO(8074910344859373$288230376151711744), d_18) # h 
;


  mp_15 = f_IL_19(CONST_RATIO(3931825344300771$36893488147419103232), CHAR, il_19) |
    f_IR_20(CONST_RATIO(3931825344300771$36893488147419103232), ir_20, CHAR) |
    f_MP_21(CONST_RATIO(121128999119993$140737488355328), CHAR, mp_21, CHAR) |
    f_ML_22(CONST_RATIO(3931825344300771$36893488147419103232), CHAR, ml_22) |
    f_MR_23(CONST_RATIO(2671385053305097$9223372036854775808), mr_23, CHAR) |
    f_D_24(CONST_RATIO(3931825344300771$36893488147419103232), d_24) # h 
;


  ml_16 = f_IL_19(CONST_RATIO(6919845182222927$288230376151711744), CHAR, il_19) |
    f_IR_20(CONST_RATIO(6919845182222927$288230376151711744), ir_20, CHAR) |
    f_MP_21(CONST_RATIO(6919845182222927$288230376151711744), CHAR, mp_21, CHAR) |
    f_ML_22(CONST_RATIO(6919845182222927$288230376151711744), CHAR, ml_22) |
    f_MR_23(CONST_RATIO(6919845182222927$288230376151711744), mr_23, CHAR) |
    f_D_24(CONST_RATIO(6919845182222927$288230376151711744), d_24) # h 
;


  mr_17 = f_IL_19(CONST_RATIO(6919845182222927$288230376151711744), CHAR, il_19) |
    f_IR_20(CONST_RATIO(6919845182222927$288230376151711744), ir_20, CHAR) |
    f_MP_21(CONST_RATIO(6919845182222927$288230376151711744), CHAR, mp_21, CHAR) |
    f_ML_22(CONST_RATIO(6919845182222927$288230376151711744), CHAR, ml_22) |
    f_MR_23(CONST_RATIO(6919845182222927$288230376151711744), mr_23, CHAR) |
    f_D_24(CONST_RATIO(6919845182222927$288230376151711744), d_24) # h 
;


  d_18 = f_IL_19(CONST_RATIO(6919845182222927$288230376151711744), CHAR, il_19) |
    f_IR_20(CONST_RATIO(6919845182222927$288230376151711744), ir_20, CHAR) |
    f_MP_21(CONST_RATIO(6919845182222927$288230376151711744), CHAR, mp_21, CHAR) |
    f_ML_22(CONST_RATIO(6919845182222927$288230376151711744), CHAR, ml_22) |
    f_MR_23(CONST_RATIO(6919845182222927$288230376151711744), mr_23, CHAR) |
    f_D_24(CONST_RATIO(6919845182222927$288230376151711744), d_24) # h 
;


  il_19 = f_IL_19(CONST_RATIO(6919845182222927$288230376151711744), CHAR, il_19) |
    f_IR_20(CONST_RATIO(6919845182222927$288230376151711744), ir_20, CHAR) |
    f_MP_21(CONST_RATIO(6919845182222927$288230376151711744), CHAR, mp_21, CHAR) |
    f_ML_22(CONST_RATIO(6919845182222927$288230376151711744), CHAR, ml_22) |
    f_MR_23(CONST_RATIO(6919845182222927$288230376151711744), mr_23, CHAR) |
    f_D_24(CONST_RATIO(6919845182222927$288230376151711744), d_24) # h 
;


  ir_20 = f_IR_20(CONST_RATIO(5056502364935393$144115188075855872), ir_20, CHAR) |
    f_MP_21(CONST_RATIO(5056502364935393$144115188075855872), CHAR, mp_21, CHAR) |
    f_ML_22(CONST_RATIO(5056502364935393$144115188075855872), CHAR, ml_22) |
    f_MR_23(CONST_RATIO(5056502364935393$144115188075855872), mr_23, CHAR) |
    f_D_24(CONST_RATIO(5056502364935393$144115188075855872), d_24) # h 
;


  mp_21 = f_IL_25(CONST_RATIO(3994719754512621$36893488147419103232), CHAR, il_25) |
    f_IR_26(CONST_RATIO(3994719754512621$36893488147419103232), ir_26, CHAR) |
    f_MP_27(CONST_RATIO(61533303651237$70368744177664), CHAR, mp_27, CHAR) |
    f_ML_28(CONST_RATIO(3994719754512621$36893488147419103232), CHAR, ml_28) |
    f_MR_29(CONST_RATIO(3994719754512621$36893488147419103232), mr_29, CHAR) |
    f_D_30(CONST_RATIO(3994719754512621$36893488147419103232), d_30) # h 
;


  ml_22 = f_IL_25(CONST_RATIO(6919845182222927$288230376151711744), CHAR, il_25) |
    f_IR_26(CONST_RATIO(6919845182222927$288230376151711744), ir_26, CHAR) |
    f_MP_27(CONST_RATIO(6919845182222927$288230376151711744), CHAR, mp_27, CHAR) |
    f_ML_28(CONST_RATIO(6919845182222927$288230376151711744), CHAR, ml_28) |
    f_MR_29(CONST_RATIO(6919845182222927$288230376151711744), mr_29, CHAR) |
    f_D_30(CONST_RATIO(6919845182222927$288230376151711744), d_30) # h 
;


  mr_23 = f_IL_25(CONST_RATIO(44110524781931$2251799813685248), CHAR, il_25) |
    f_IR_26(CONST_RATIO(44110524781931$2251799813685248), ir_26, CHAR) |
    f_MP_27(CONST_RATIO(7672280349958849$144115188075855872), CHAR, mp_27, CHAR) |
    f_ML_28(CONST_RATIO(44110524781931$2251799813685248), CHAR, ml_28) |
    f_MR_29(CONST_RATIO(44110524781931$2251799813685248), mr_29, CHAR) |
    f_D_30(CONST_RATIO(44110524781931$2251799813685248), d_30) # h 
;


  d_24 = f_IL_25(CONST_RATIO(6919845182222927$288230376151711744), CHAR, il_25) |
    f_IR_26(CONST_RATIO(6919845182222927$288230376151711744), ir_26, CHAR) |
    f_MP_27(CONST_RATIO(6919845182222927$288230376151711744), CHAR, mp_27, CHAR) |
    f_ML_28(CONST_RATIO(6919845182222927$288230376151711744), CHAR, ml_28) |
    f_MR_29(CONST_RATIO(6919845182222927$288230376151711744), mr_29, CHAR) |
    f_D_30(CONST_RATIO(6919845182222927$288230376151711744), d_30) # h 
;


  il_25 = f_IL_25(CONST_RATIO(6919845182222927$288230376151711744), CHAR, il_25) |
    f_IR_26(CONST_RATIO(6919845182222927$288230376151711744), ir_26, CHAR) |
    f_MP_27(CONST_RATIO(6919845182222927$288230376151711744), CHAR, mp_27, CHAR) |
    f_ML_28(CONST_RATIO(6919845182222927$288230376151711744), CHAR, ml_28) |
    f_MR_29(CONST_RATIO(6919845182222927$288230376151711744), mr_29, CHAR) |
    f_D_30(CONST_RATIO(6919845182222927$288230376151711744), d_30) # h 
;


  ir_26 = f_IR_26(CONST_RATIO(5056502364935393$144115188075855872), ir_26, CHAR) |
    f_MP_27(CONST_RATIO(5056502364935393$144115188075855872), CHAR, mp_27, CHAR) |
    f_ML_28(CONST_RATIO(5056502364935393$144115188075855872), CHAR, ml_28) |
    f_MR_29(CONST_RATIO(5056502364935393$144115188075855872), mr_29, CHAR) |
    f_D_30(CONST_RATIO(5056502364935393$144115188075855872), d_30) # h 
;


  mp_27 = f_IL_31(CONST_RATIO(3931825344300771$36893488147419103232), CHAR, il_31) |
    f_IR_32(CONST_RATIO(3931825344300771$36893488147419103232), ir_32, CHAR) |
    f_MP_33(CONST_RATIO(985954264173203$1125899906842624), CHAR, mp_33, CHAR) |
    f_ML_34(CONST_RATIO(3931825344300771$36893488147419103232), CHAR, ml_34) |
    f_MR_35(CONST_RATIO(3931825344300771$36893488147419103232), mr_35, CHAR) |
    f_D_36(CONST_RATIO(3931825344300771$36893488147419103232), d_36) # h 
;


  ml_28 = f_IL_31(CONST_RATIO(6919845182222927$288230376151711744), CHAR, il_31) |
    f_IR_32(CONST_RATIO(6919845182222927$288230376151711744), ir_32, CHAR) |
    f_MP_33(CONST_RATIO(6919845182222927$288230376151711744), CHAR, mp_33, CHAR) |
    f_ML_34(CONST_RATIO(6919845182222927$288230376151711744), CHAR, ml_34) |
    f_MR_35(CONST_RATIO(6919845182222927$288230376151711744), mr_35, CHAR) |
    f_D_36(CONST_RATIO(6919845182222927$288230376151711744), d_36) # h 
;


  mr_29 = f_IL_31(CONST_RATIO(6919845182222927$288230376151711744), CHAR, il_31) |
    f_IR_32(CONST_RATIO(6919845182222927$288230376151711744), ir_32, CHAR) |
    f_MP_33(CONST_RATIO(6919845182222927$288230376151711744), CHAR, mp_33, CHAR) |
    f_ML_34(CONST_RATIO(6919845182222927$288230376151711744), CHAR, ml_34) |
    f_MR_35(CONST_RATIO(6919845182222927$288230376151711744), mr_35, CHAR) |
    f_D_36(CONST_RATIO(6919845182222927$288230376151711744), d_36) # h 
;


  d_30 = f_IL_31(CONST_RATIO(6919845182222927$288230376151711744), CHAR, il_31) |
    f_IR_32(CONST_RATIO(6919845182222927$288230376151711744), ir_32, CHAR) |
    f_MP_33(CONST_RATIO(6919845182222927$288230376151711744), CHAR, mp_33, CHAR) |
    f_ML_34(CONST_RATIO(6919845182222927$288230376151711744), CHAR, ml_34) |
    f_MR_35(CONST_RATIO(6919845182222927$288230376151711744), mr_35, CHAR) |
    f_D_36(CONST_RATIO(6919845182222927$288230376151711744), d_36) # h 
;


  il_31 = f_IL_31(CONST_RATIO(6919845182222927$288230376151711744), CHAR, il_31) |
    f_IR_32(CONST_RATIO(6919845182222927$288230376151711744), ir_32, CHAR) |
    f_MP_33(CONST_RATIO(6919845182222927$288230376151711744), CHAR, mp_33, CHAR) |
    f_ML_34(CONST_RATIO(6919845182222927$288230376151711744), CHAR, ml_34) |
    f_MR_35(CONST_RATIO(6919845182222927$288230376151711744), mr_35, CHAR) |
    f_D_36(CONST_RATIO(6919845182222927$288230376151711744), d_36) # h 
;


  ir_32 = f_IR_32(CONST_RATIO(5056502364935393$144115188075855872), ir_32, CHAR) |
    f_MP_33(CONST_RATIO(5056502364935393$144115188075855872), CHAR, mp_33, CHAR) |
    f_ML_34(CONST_RATIO(5056502364935393$144115188075855872), CHAR, ml_34) |
    f_MR_35(CONST_RATIO(5056502364935393$144115188075855872), mr_35, CHAR) |
    f_D_36(CONST_RATIO(5056502364935393$144115188075855872), d_36) # h 
;


  mp_33 = f_IL_37(CONST_RATIO(8282857224389311$73786976294838206464), CHAR, il_37) |
    f_IR_38(CONST_RATIO(2535212977759739$288230376151711744), ir_38, CHAR) |
    f_ML_39(CONST_RATIO(3153557936587935$4503599627370496), CHAR, ml_39) |
    f_D_40(CONST_RATIO(6181692104627111$4611686018427387904), d_40) # h 
;


  ml_34 = f_IL_37(CONST_RATIO(8046384138893273$144115188075855872), CHAR, il_37) |
    f_IR_38(CONST_RATIO(8046384138893273$144115188075855872), ir_38, CHAR) |
    f_ML_39(CONST_RATIO(8046384138893273$144115188075855872), CHAR, ml_39) |
    f_D_40(CONST_RATIO(8046384138893273$144115188075855872), d_40) # h 
;


  mr_35 = f_IL_37(CONST_RATIO(8046384138893273$144115188075855872), CHAR, il_37) |
    f_IR_38(CONST_RATIO(8046384138893273$144115188075855872), ir_38, CHAR) |
    f_ML_39(CONST_RATIO(8046384138893273$144115188075855872), CHAR, ml_39) |
    f_D_40(CONST_RATIO(8046384138893273$144115188075855872), d_40) # h 
;


  d_36 = f_IL_37(CONST_RATIO(8046384138893273$144115188075855872), CHAR, il_37) |
    f_IR_38(CONST_RATIO(8046384138893273$144115188075855872), ir_38, CHAR) |
    f_ML_39(CONST_RATIO(8046384138893273$144115188075855872), CHAR, ml_39) |
    f_D_40(CONST_RATIO(8046384138893273$144115188075855872), d_40) # h 
;


  il_37 = f_IL_37(CONST_RATIO(8046384138893273$144115188075855872), CHAR, il_37) |
    f_IR_38(CONST_RATIO(8046384138893273$144115188075855872), ir_38, CHAR) |
    f_ML_39(CONST_RATIO(8046384138893273$144115188075855872), CHAR, ml_39) |
    f_D_40(CONST_RATIO(8046384138893273$144115188075855872), d_40) # h 
;


  ir_38 = f_IR_38(CONST_RATIO(6783321051604181$18014398509481984), ir_38, CHAR) |
    f_ML_39(CONST_RATIO(7321345163802901$72057594037927936), CHAR, ml_39) |
    f_D_40(CONST_RATIO(2989968555612195$2305843009213693952), d_40) # h 
;


  ml_39 = f_IL_41(CONST_RATIO(2264469954094969$18446744073709551616), CHAR, il_41) |
    f_ML_42(CONST_RATIO(8514401876804273$9007199254740992), CHAR, ml_42) |
    f_D_43(CONST_RATIO(2264469954094969$18446744073709551616), d_43) # h 
;


  d_40 = f_IL_41(CONST_RATIO(4492341323360557$144115188075855872), CHAR, il_41) |
    f_ML_42(CONST_RATIO(209546281385675$562949953421312), CHAR, ml_42) |
    f_D_43(CONST_RATIO(4492341323360557$144115188075855872), d_43) # h 
;


  il_41 = f_IL_41(CONST_RATIO(7321345163802901$72057594037927936), CHAR, il_41) |
    f_ML_42(CONST_RATIO(7321345163802901$72057594037927936), CHAR, ml_42) |
    f_D_43(CONST_RATIO(7321345163802901$72057594037927936), d_43) # h 
;


  ml_42 = f_IL_44(CONST_RATIO(4256520359554969$36893488147419103232), CHAR, il_44) |
    f_ML_45(CONST_RATIO(2134751181217393$2251799813685248), CHAR, ml_45) |
    f_D_46(CONST_RATIO(4256520359554969$36893488147419103232), d_46) # h 
;


  d_43 = f_IL_44(CONST_RATIO(7321345163802901$72057594037927936), CHAR, il_44) |
    f_ML_45(CONST_RATIO(7321345163802901$72057594037927936), CHAR, ml_45) |
    f_D_46(CONST_RATIO(7321345163802901$72057594037927936), d_46) # h 
;


  il_44 = f_IL_44(CONST_RATIO(7321345163802901$72057594037927936), CHAR, il_44) |
    f_ML_45(CONST_RATIO(7321345163802901$72057594037927936), CHAR, ml_45) |
    f_D_46(CONST_RATIO(7321345163802901$72057594037927936), d_46) # h 
;


  ml_45 = f_IL_47(CONST_RATIO(4256520359554969$36893488147419103232), CHAR, il_47) |
    f_ML_48(CONST_RATIO(2134751181217393$2251799813685248), CHAR, ml_48) |
    f_D_49(CONST_RATIO(4256520359554969$36893488147419103232), d_49) # h 
;


  d_46 = f_IL_47(CONST_RATIO(7321345163802901$72057594037927936), CHAR, il_47) |
    f_ML_48(CONST_RATIO(7321345163802901$72057594037927936), CHAR, ml_48) |
    f_D_49(CONST_RATIO(7321345163802901$72057594037927936), d_49) # h 
;


  il_47 = f_IL_47(CONST_RATIO(7321345163802901$72057594037927936), CHAR, il_47) |
    f_ML_48(CONST_RATIO(7321345163802901$72057594037927936), CHAR, ml_48) |
    f_D_49(CONST_RATIO(7321345163802901$72057594037927936), d_49) # h 
;


  ml_48 = f_IL_50(CONST_RATIO(4256520359554969$36893488147419103232), CHAR, il_50) |
    f_ML_51(CONST_RATIO(2134751181217393$2251799813685248), CHAR, ml_51) |
    f_D_52(CONST_RATIO(4256520359554969$36893488147419103232), d_52) # h 
;


  d_49 = f_IL_50(CONST_RATIO(7321345163802901$72057594037927936), CHAR, il_50) |
    f_ML_51(CONST_RATIO(7321345163802901$72057594037927936), CHAR, ml_51) |
    f_D_52(CONST_RATIO(7321345163802901$72057594037927936), d_52) # h 
;


  il_50 = f_IL_50(CONST_RATIO(7321345163802901$72057594037927936), CHAR, il_50) |
    f_ML_51(CONST_RATIO(7321345163802901$72057594037927936), CHAR, ml_51) |
    f_D_52(CONST_RATIO(7321345163802901$72057594037927936), d_52) # h 
;


  ml_51 = f_IL_53(CONST_RATIO(4256520359554969$36893488147419103232), CHAR, il_53) |
    f_ML_54(CONST_RATIO(2134751181217393$2251799813685248), CHAR, ml_54) |
    f_D_55(CONST_RATIO(4256520359554969$36893488147419103232), d_55) # h 
;


  d_52 = f_IL_53(CONST_RATIO(7321345163802901$72057594037927936), CHAR, il_53) |
    f_ML_54(CONST_RATIO(7321345163802901$72057594037927936), CHAR, ml_54) |
    f_D_55(CONST_RATIO(7321345163802901$72057594037927936), d_55) # h 
;


  il_53 = f_IL_53(CONST_RATIO(7321345163802901$72057594037927936), CHAR, il_53) |
    f_ML_54(CONST_RATIO(7321345163802901$72057594037927936), CHAR, ml_54) |
    f_D_55(CONST_RATIO(7321345163802901$72057594037927936), d_55) # h 
;


  ml_54 = f_IL_56(CONST_RATIO(4256520359554969$36893488147419103232), CHAR, il_56) |
    f_ML_57(CONST_RATIO(2134751181217393$2251799813685248), CHAR, ml_57) |
    f_D_58(CONST_RATIO(4256520359554969$36893488147419103232), d_58) # h 
;


  d_55 = f_IL_56(CONST_RATIO(7321345163802901$72057594037927936), CHAR, il_56) |
    f_ML_57(CONST_RATIO(7321345163802901$72057594037927936), CHAR, ml_57) |
    f_D_58(CONST_RATIO(7321345163802901$72057594037927936), d_58) # h 
;


  il_56 = f_IL_56(CONST_RATIO(7321345163802901$72057594037927936), CHAR, il_56) |
    f_ML_57(CONST_RATIO(7321345163802901$72057594037927936), CHAR, ml_57) |
    f_D_58(CONST_RATIO(7321345163802901$72057594037927936), d_58) # h 
;


  ml_57 = f_IL_59(CONST_RATIO(4256520359554969$36893488147419103232), CHAR, il_59) |
    f_MR_60(CONST_RATIO(4190173923084815$4503599627370496), mr_60, CHAR) |
    f_D_61(CONST_RATIO(3041770168270021$9223372036854775808), d_61) # h 
;


  d_58 = f_IL_59(CONST_RATIO(7321345163802901$72057594037927936), CHAR, il_59) |
    f_MR_60(CONST_RATIO(7321345163802901$72057594037927936), mr_60, CHAR) |
    f_D_61(CONST_RATIO(7321345163802901$72057594037927936), d_61) # h 
;


  il_59 = f_IL_59(CONST_RATIO(7321345163802901$72057594037927936), CHAR, il_59) |
    f_MR_60(CONST_RATIO(7321345163802901$72057594037927936), mr_60, CHAR) |
    f_D_61(CONST_RATIO(7321345163802901$72057594037927936), d_61) # h 
;


  mr_60 = f_IR_62(CONST_RATIO(8902416318285045$73786976294838206464), ir_62, CHAR) |
    b_63 # h 
;


  d_61 = f_IR_62(CONST_RATIO(1178007164480647$9007199254740992), ir_62, CHAR) |
    b_63 # h 
;


  ir_62 = f_IR_62(CONST_RATIO(532077976909867$2251799813685248), ir_62, CHAR) |
    b_63 # h 
;


  b_63 = f_B_63(s_100, s_64) 
;


  s_64 = f_IL_65(CONST_RATIO(4256520359554969$36893488147419103232), CHAR, il_65) |
    f_ML_66(CONST_RATIO(2134751181217393$2251799813685248), CHAR, ml_66) |
    f_D_67(CONST_RATIO(4256520359554969$36893488147419103232), d_67) # h 
;


  il_65 = f_IL_65(CONST_RATIO(7321345163802901$72057594037927936), CHAR, il_65) |
    f_ML_66(CONST_RATIO(7321345163802901$72057594037927936), CHAR, ml_66) |
    f_D_67(CONST_RATIO(7321345163802901$72057594037927936), d_67) # h 
;


  ml_66 = f_IL_68(CONST_RATIO(4256520359554969$36893488147419103232), CHAR, il_68) |
    f_ML_69(CONST_RATIO(2134751181217393$2251799813685248), CHAR, ml_69) |
    f_D_70(CONST_RATIO(4256520359554969$36893488147419103232), d_70) # h 
;


  d_67 = f_IL_68(CONST_RATIO(7321345163802901$72057594037927936), CHAR, il_68) |
    f_ML_69(CONST_RATIO(7321345163802901$72057594037927936), CHAR, ml_69) |
    f_D_70(CONST_RATIO(7321345163802901$72057594037927936), d_70) # h 
;


  il_68 = f_IL_68(CONST_RATIO(7321345163802901$72057594037927936), CHAR, il_68) |
    f_ML_69(CONST_RATIO(7321345163802901$72057594037927936), CHAR, ml_69) |
    f_D_70(CONST_RATIO(7321345163802901$72057594037927936), d_70) # h 
;


  ml_69 = f_IL_71(CONST_RATIO(4256520359554969$36893488147419103232), CHAR, il_71) |
    f_ML_72(CONST_RATIO(2026708554958921$2251799813685248), CHAR, ml_72) |
    f_D_73(CONST_RATIO(2380515484528567$2305843009213693952), d_73) # h 
;


  d_70 = f_IL_71(CONST_RATIO(7321345163802901$72057594037927936), CHAR, il_71) |
    f_ML_72(CONST_RATIO(7321345163802901$72057594037927936), CHAR, ml_72) |
    f_D_73(CONST_RATIO(7321345163802901$72057594037927936), d_73) # h 
;


  il_71 = f_IL_71(CONST_RATIO(7321345163802901$72057594037927936), CHAR, il_71) |
    f_ML_72(CONST_RATIO(7321345163802901$72057594037927936), CHAR, ml_72) |
    f_D_73(CONST_RATIO(7321345163802901$72057594037927936), d_73) # h 
;


  ml_72 = f_IL_74(CONST_RATIO(2337812305228147$4611686018427387904), CHAR, il_74) |
    f_MP_75(CONST_RATIO(7842247395248343$9007199254740992), CHAR, mp_75, CHAR) |
    f_ML_76(CONST_RATIO(4238137586910837$36893488147419103232), CHAR, ml_76) |
    f_MR_77(CONST_RATIO(4238137586910837$36893488147419103232), mr_77, CHAR) |
    f_D_78(CONST_RATIO(4238137586910837$36893488147419103232), d_78) # h 
;


  d_73 = f_IL_74(CONST_RATIO(5230519264668049$288230376151711744), CHAR, il_74) |
    f_MP_75(CONST_RATIO(2925237294758075$18014398509481984), CHAR, mp_75, CHAR) |
    f_ML_76(CONST_RATIO(5230519264668049$288230376151711744), CHAR, ml_76) |
    f_MR_77(CONST_RATIO(5230519264668049$288230376151711744), mr_77, CHAR) |
    f_D_78(CONST_RATIO(5230519264668049$288230376151711744), d_78) # h 
;


  il_74 = f_IL_74(CONST_RATIO(2397157566487669$18014398509481984), CHAR, il_74) |
    f_MP_75(CONST_RATIO(2032013796673279$36028797018963968), CHAR, mp_75, CHAR) |
    f_ML_76(CONST_RATIO(3683766241431425$288230376151711744), CHAR, ml_76) |
    f_MR_77(CONST_RATIO(3683766241431425$288230376151711744), mr_77, CHAR) |
    f_D_78(CONST_RATIO(3683766241431425$288230376151711744), d_78) # h 
;


  mp_75 = f_IL_79(CONST_RATIO(3931825344300771$36893488147419103232), CHAR, il_79) |
    f_IR_80(CONST_RATIO(3931825344300771$36893488147419103232), ir_80, CHAR) |
    f_MP_81(CONST_RATIO(985954264173203$1125899906842624), CHAR, mp_81, CHAR) |
    f_ML_82(CONST_RATIO(3931825344300771$36893488147419103232), CHAR, ml_82) |
    f_MR_83(CONST_RATIO(3931825344300771$36893488147419103232), mr_83, CHAR) |
    f_D_84(CONST_RATIO(3931825344300771$36893488147419103232), d_84) # h 
;


  ml_76 = f_IL_79(CONST_RATIO(6919845182222927$288230376151711744), CHAR, il_79) |
    f_IR_80(CONST_RATIO(6919845182222927$288230376151711744), ir_80, CHAR) |
    f_MP_81(CONST_RATIO(6919845182222927$288230376151711744), CHAR, mp_81, CHAR) |
    f_ML_82(CONST_RATIO(6919845182222927$288230376151711744), CHAR, ml_82) |
    f_MR_83(CONST_RATIO(6919845182222927$288230376151711744), mr_83, CHAR) |
    f_D_84(CONST_RATIO(6919845182222927$288230376151711744), d_84) # h 
;


  mr_77 = f_IL_79(CONST_RATIO(6919845182222927$288230376151711744), CHAR, il_79) |
    f_IR_80(CONST_RATIO(6919845182222927$288230376151711744), ir_80, CHAR) |
    f_MP_81(CONST_RATIO(6919845182222927$288230376151711744), CHAR, mp_81, CHAR) |
    f_ML_82(CONST_RATIO(6919845182222927$288230376151711744), CHAR, ml_82) |
    f_MR_83(CONST_RATIO(6919845182222927$288230376151711744), mr_83, CHAR) |
    f_D_84(CONST_RATIO(6919845182222927$288230376151711744), d_84) # h 
;


  d_78 = f_IL_79(CONST_RATIO(6919845182222927$288230376151711744), CHAR, il_79) |
    f_IR_80(CONST_RATIO(6919845182222927$288230376151711744), ir_80, CHAR) |
    f_MP_81(CONST_RATIO(6919845182222927$288230376151711744), CHAR, mp_81, CHAR) |
    f_ML_82(CONST_RATIO(6919845182222927$288230376151711744), CHAR, ml_82) |
    f_MR_83(CONST_RATIO(6919845182222927$288230376151711744), mr_83, CHAR) |
    f_D_84(CONST_RATIO(6919845182222927$288230376151711744), d_84) # h 
;


  il_79 = f_IL_79(CONST_RATIO(6919845182222927$288230376151711744), CHAR, il_79) |
    f_IR_80(CONST_RATIO(6919845182222927$288230376151711744), ir_80, CHAR) |
    f_MP_81(CONST_RATIO(6919845182222927$288230376151711744), CHAR, mp_81, CHAR) |
    f_ML_82(CONST_RATIO(6919845182222927$288230376151711744), CHAR, ml_82) |
    f_MR_83(CONST_RATIO(6919845182222927$288230376151711744), mr_83, CHAR) |
    f_D_84(CONST_RATIO(6919845182222927$288230376151711744), d_84) # h 
;


  ir_80 = f_IR_80(CONST_RATIO(5056502364935393$144115188075855872), ir_80, CHAR) |
    f_MP_81(CONST_RATIO(5056502364935393$144115188075855872), CHAR, mp_81, CHAR) |
    f_ML_82(CONST_RATIO(5056502364935393$144115188075855872), CHAR, ml_82) |
    f_MR_83(CONST_RATIO(5056502364935393$144115188075855872), mr_83, CHAR) |
    f_D_84(CONST_RATIO(5056502364935393$144115188075855872), d_84) # h 
;


  mp_81 = f_IL_85(CONST_RATIO(3931825344300771$36893488147419103232), CHAR, il_85) |
    f_IR_86(CONST_RATIO(3931825344300771$36893488147419103232), ir_86, CHAR) |
    f_MP_87(CONST_RATIO(1924134111218175$2251799813685248), CHAR, mp_87, CHAR) |
    f_ML_88(CONST_RATIO(3931825344300771$36893488147419103232), CHAR, ml_88) |
    f_MR_89(CONST_RATIO(7542459932439835$18446744073709551616), mr_89, CHAR) |
    f_D_90(CONST_RATIO(3931825344300771$36893488147419103232), d_90) # h 
;


  ml_82 = f_IL_85(CONST_RATIO(6919845182222927$288230376151711744), CHAR, il_85) |
    f_IR_86(CONST_RATIO(6919845182222927$288230376151711744), ir_86, CHAR) |
    f_MP_87(CONST_RATIO(6919845182222927$288230376151711744), CHAR, mp_87, CHAR) |
    f_ML_88(CONST_RATIO(6919845182222927$288230376151711744), CHAR, ml_88) |
    f_MR_89(CONST_RATIO(6919845182222927$288230376151711744), mr_89, CHAR) |
    f_D_90(CONST_RATIO(6919845182222927$288230376151711744), d_90) # h 
;


  mr_83 = f_IL_85(CONST_RATIO(6919845182222927$288230376151711744), CHAR, il_85) |
    f_IR_86(CONST_RATIO(6919845182222927$288230376151711744), ir_86, CHAR) |
    f_MP_87(CONST_RATIO(6919845182222927$288230376151711744), CHAR, mp_87, CHAR) |
    f_ML_88(CONST_RATIO(6919845182222927$288230376151711744), CHAR, ml_88) |
    f_MR_89(CONST_RATIO(6919845182222927$288230376151711744), mr_89, CHAR) |
    f_D_90(CONST_RATIO(6919845182222927$288230376151711744), d_90) # h 
;


  d_84 = f_IL_85(CONST_RATIO(6919845182222927$288230376151711744), CHAR, il_85) |
    f_IR_86(CONST_RATIO(6919845182222927$288230376151711744), ir_86, CHAR) |
    f_MP_87(CONST_RATIO(6919845182222927$288230376151711744), CHAR, mp_87, CHAR) |
    f_ML_88(CONST_RATIO(6919845182222927$288230376151711744), CHAR, ml_88) |
    f_MR_89(CONST_RATIO(6919845182222927$288230376151711744), mr_89, CHAR) |
    f_D_90(CONST_RATIO(6919845182222927$288230376151711744), d_90) # h 
;


  il_85 = f_IL_85(CONST_RATIO(6919845182222927$288230376151711744), CHAR, il_85) |
    f_IR_86(CONST_RATIO(6919845182222927$288230376151711744), ir_86, CHAR) |
    f_MP_87(CONST_RATIO(6919845182222927$288230376151711744), CHAR, mp_87, CHAR) |
    f_ML_88(CONST_RATIO(6919845182222927$288230376151711744), CHAR, ml_88) |
    f_MR_89(CONST_RATIO(6919845182222927$288230376151711744), mr_89, CHAR) |
    f_D_90(CONST_RATIO(6919845182222927$288230376151711744), d_90) # h 
;


  ir_86 = f_IR_86(CONST_RATIO(5056502364935393$144115188075855872), ir_86, CHAR) |
    f_MP_87(CONST_RATIO(5056502364935393$144115188075855872), CHAR, mp_87, CHAR) |
    f_ML_88(CONST_RATIO(5056502364935393$144115188075855872), CHAR, ml_88) |
    f_MR_89(CONST_RATIO(5056502364935393$144115188075855872), mr_89, CHAR) |
    f_D_90(CONST_RATIO(5056502364935393$144115188075855872), d_90) # h 
;


  mp_87 = f_IL_91(CONST_RATIO(2011819873917767$18446744073709551616), CHAR, il_91) |
    f_IR_92(CONST_RATIO(2011819873917767$18446744073709551616), ir_92, CHAR) |
    f_MP_93(CONST_RATIO(7674360942822591$9007199254740992), CHAR, mp_93, CHAR) |
    f_ML_94(CONST_RATIO(2011819873917767$18446744073709551616), CHAR, ml_94) |
    f_MR_95(CONST_RATIO(2011819873917767$18446744073709551616), mr_95, CHAR) |
    f_D_96(CONST_RATIO(4024185282427893$9223372036854775808), d_96) # h 
;


  ml_88 = f_IL_91(CONST_RATIO(6919845182222927$288230376151711744), CHAR, il_91) |
    f_IR_92(CONST_RATIO(6919845182222927$288230376151711744), ir_92, CHAR) |
    f_MP_93(CONST_RATIO(6919845182222927$288230376151711744), CHAR, mp_93, CHAR) |
    f_ML_94(CONST_RATIO(6919845182222927$288230376151711744), CHAR, ml_94) |
    f_MR_95(CONST_RATIO(6919845182222927$288230376151711744), mr_95, CHAR) |
    f_D_96(CONST_RATIO(6919845182222927$288230376151711744), d_96) # h 
;


  mr_89 = f_IL_91(CONST_RATIO(5163043873657659$288230376151711744), CHAR, il_91) |
    f_IR_92(CONST_RATIO(5163043873657659$288230376151711744), ir_92, CHAR) |
    f_MP_93(CONST_RATIO(1236255019730095$18014398509481984), CHAR, mp_93, CHAR) |
    f_ML_94(CONST_RATIO(5163043873657659$288230376151711744), CHAR, ml_94) |
    f_MR_95(CONST_RATIO(5163043873657659$288230376151711744), mr_95, CHAR) |
    f_D_96(CONST_RATIO(5163043873657659$288230376151711744), d_96) # h 
;


  d_90 = f_IL_91(CONST_RATIO(6919845182222927$288230376151711744), CHAR, il_91) |
    f_IR_92(CONST_RATIO(6919845182222927$288230376151711744), ir_92, CHAR) |
    f_MP_93(CONST_RATIO(6919845182222927$288230376151711744), CHAR, mp_93, CHAR) |
    f_ML_94(CONST_RATIO(6919845182222927$288230376151711744), CHAR, ml_94) |
    f_MR_95(CONST_RATIO(6919845182222927$288230376151711744), mr_95, CHAR) |
    f_D_96(CONST_RATIO(6919845182222927$288230376151711744), d_96) # h 
;


  il_91 = f_IL_91(CONST_RATIO(6919845182222927$288230376151711744), CHAR, il_91) |
    f_IR_92(CONST_RATIO(6919845182222927$288230376151711744), ir_92, CHAR) |
    f_MP_93(CONST_RATIO(6919845182222927$288230376151711744), CHAR, mp_93, CHAR) |
    f_ML_94(CONST_RATIO(6919845182222927$288230376151711744), CHAR, ml_94) |
    f_MR_95(CONST_RATIO(6919845182222927$288230376151711744), mr_95, CHAR) |
    f_D_96(CONST_RATIO(6919845182222927$288230376151711744), d_96) # h 
;


  ir_92 = f_IR_92(CONST_RATIO(5056502364935393$144115188075855872), ir_92, CHAR) |
    f_MP_93(CONST_RATIO(5056502364935393$144115188075855872), CHAR, mp_93, CHAR) |
    f_ML_94(CONST_RATIO(5056502364935393$144115188075855872), CHAR, ml_94) |
    f_MR_95(CONST_RATIO(5056502364935393$144115188075855872), mr_95, CHAR) |
    f_D_96(CONST_RATIO(5056502364935393$144115188075855872), d_96) # h 
;


  mp_93 = f_E_99(CONST_RATIO(1873331523897447$4503599627370496), e_99) # h 
;


  ml_94 = f_E_99(CONST_RATIO(7321345163802901$72057594037927936), e_99) # h 
;


  mr_95 = f_E_99(CONST_RATIO(7321345163802901$72057594037927936), e_99) # h 
;


  d_96 = f_E_99(CONST_RATIO(8271117229599445$36028797018963968), e_99) # h 
;


  e_99 = nil(EMPTY) 
;


  s_100 = f_MP_101(CONST_RATIO(519257449454229$562949953421312), CHAR, mp_101, CHAR) |
    f_ML_102(CONST_RATIO(8282857224389311$73786976294838206464), CHAR, ml_102) |
    f_MR_103(CONST_RATIO(8282857224389311$73786976294838206464), mr_103, CHAR) |
    f_D_104(CONST_RATIO(8282857224389311$73786976294838206464), d_104) # h 
;


  mp_101 = f_IL_105(CONST_RATIO(3931825344300771$36893488147419103232), CHAR, il_105) |
    f_IR_106(CONST_RATIO(3931825344300771$36893488147419103232), ir_106, CHAR) |
    f_MP_107(CONST_RATIO(7797121839603345$9007199254740992), CHAR, mp_107, CHAR) |
    f_ML_108(CONST_RATIO(2054487832635289$9223372036854775808), CHAR, ml_108) |
    f_MR_109(CONST_RATIO(3931825344300771$36893488147419103232), mr_109, CHAR) |
    f_D_110(CONST_RATIO(3931825344300771$36893488147419103232), d_110) # h 
;


  ml_102 = f_IL_105(CONST_RATIO(6919845182222927$288230376151711744), CHAR, il_105) |
    f_IR_106(CONST_RATIO(6919845182222927$288230376151711744), ir_106, CHAR) |
    f_MP_107(CONST_RATIO(6919845182222927$288230376151711744), CHAR, mp_107, CHAR) |
    f_ML_108(CONST_RATIO(6919845182222927$288230376151711744), CHAR, ml_108) |
    f_MR_109(CONST_RATIO(6919845182222927$288230376151711744), mr_109, CHAR) |
    f_D_110(CONST_RATIO(6919845182222927$288230376151711744), d_110) # h 
;


  mr_103 = f_IL_105(CONST_RATIO(6919845182222927$288230376151711744), CHAR, il_105) |
    f_IR_106(CONST_RATIO(6919845182222927$288230376151711744), ir_106, CHAR) |
    f_MP_107(CONST_RATIO(6919845182222927$288230376151711744), CHAR, mp_107, CHAR) |
    f_ML_108(CONST_RATIO(6919845182222927$288230376151711744), CHAR, ml_108) |
    f_MR_109(CONST_RATIO(6919845182222927$288230376151711744), mr_109, CHAR) |
    f_D_110(CONST_RATIO(6919845182222927$288230376151711744), d_110) # h 
;


  d_104 = f_IL_105(CONST_RATIO(6919845182222927$288230376151711744), CHAR, il_105) |
    f_IR_106(CONST_RATIO(6919845182222927$288230376151711744), ir_106, CHAR) |
    f_MP_107(CONST_RATIO(6919845182222927$288230376151711744), CHAR, mp_107, CHAR) |
    f_ML_108(CONST_RATIO(6919845182222927$288230376151711744), CHAR, ml_108) |
    f_MR_109(CONST_RATIO(6919845182222927$288230376151711744), mr_109, CHAR) |
    f_D_110(CONST_RATIO(6919845182222927$288230376151711744), d_110) # h 
;


  il_105 = f_IL_105(CONST_RATIO(6919845182222927$288230376151711744), CHAR, il_105) |
    f_IR_106(CONST_RATIO(6919845182222927$288230376151711744), ir_106, CHAR) |
    f_MP_107(CONST_RATIO(6919845182222927$288230376151711744), CHAR, mp_107, CHAR) |
    f_ML_108(CONST_RATIO(6919845182222927$288230376151711744), CHAR, ml_108) |
    f_MR_109(CONST_RATIO(6919845182222927$288230376151711744), mr_109, CHAR) |
    f_D_110(CONST_RATIO(6919845182222927$288230376151711744), d_110) # h 
;


  ir_106 = f_IR_106(CONST_RATIO(5056502364935393$144115188075855872), ir_106, CHAR) |
    f_MP_107(CONST_RATIO(5056502364935393$144115188075855872), CHAR, mp_107, CHAR) |
    f_ML_108(CONST_RATIO(5056502364935393$144115188075855872), CHAR, ml_108) |
    f_MR_109(CONST_RATIO(5056502364935393$144115188075855872), mr_109, CHAR) |
    f_D_110(CONST_RATIO(5056502364935393$144115188075855872), d_110) # h 
;


  mp_107 = f_IL_111(CONST_RATIO(1988733814319787$18446744073709551616), CHAR, il_111) |
    f_IR_112(CONST_RATIO(1988733814319787$18446744073709551616), ir_112, CHAR) |
    f_MP_113(CONST_RATIO(985954264173203$1125899906842624), CHAR, mp_113, CHAR) |
    f_ML_114(CONST_RATIO(1988733814319787$18446744073709551616), CHAR, ml_114) |
    f_MR_115(CONST_RATIO(1988733814319787$18446744073709551616), mr_115, CHAR) |
    f_D_116(CONST_RATIO(1988733814319787$18446744073709551616), d_116) # h 
;


  ml_108 = f_IL_111(CONST_RATIO(2999421411577597$144115188075855872), CHAR, il_111) |
    f_IR_112(CONST_RATIO(2999421411577597$144115188075855872), ir_112, CHAR) |
    f_MP_113(CONST_RATIO(6269123631306995$144115188075855872), CHAR, mp_113, CHAR) |
    f_ML_114(CONST_RATIO(2999421411577597$144115188075855872), CHAR, ml_114) |
    f_MR_115(CONST_RATIO(2999421411577597$144115188075855872), mr_115, CHAR) |
    f_D_116(CONST_RATIO(2999421411577597$144115188075855872), d_116) # h 
;


  mr_109 = f_IL_111(CONST_RATIO(6919845182222927$288230376151711744), CHAR, il_111) |
    f_IR_112(CONST_RATIO(6919845182222927$288230376151711744), ir_112, CHAR) |
    f_MP_113(CONST_RATIO(6919845182222927$288230376151711744), CHAR, mp_113, CHAR) |
    f_ML_114(CONST_RATIO(6919845182222927$288230376151711744), CHAR, ml_114) |
    f_MR_115(CONST_RATIO(6919845182222927$288230376151711744), mr_115, CHAR) |
    f_D_116(CONST_RATIO(6919845182222927$288230376151711744), d_116) # h 
;


  d_110 = f_IL_111(CONST_RATIO(6919845182222927$288230376151711744), CHAR, il_111) |
    f_IR_112(CONST_RATIO(6919845182222927$288230376151711744), ir_112, CHAR) |
    f_MP_113(CONST_RATIO(6919845182222927$288230376151711744), CHAR, mp_113, CHAR) |
    f_ML_114(CONST_RATIO(6919845182222927$288230376151711744), CHAR, ml_114) |
    f_MR_115(CONST_RATIO(6919845182222927$288230376151711744), mr_115, CHAR) |
    f_D_116(CONST_RATIO(6919845182222927$288230376151711744), d_116) # h 
;


  il_111 = f_IL_111(CONST_RATIO(6919845182222927$288230376151711744), CHAR, il_111) |
    f_IR_112(CONST_RATIO(6919845182222927$288230376151711744), ir_112, CHAR) |
    f_MP_113(CONST_RATIO(6919845182222927$288230376151711744), CHAR, mp_113, CHAR) |
    f_ML_114(CONST_RATIO(6919845182222927$288230376151711744), CHAR, ml_114) |
    f_MR_115(CONST_RATIO(6919845182222927$288230376151711744), mr_115, CHAR) |
    f_D_116(CONST_RATIO(6919845182222927$288230376151711744), d_116) # h 
;


  ir_112 = f_IR_112(CONST_RATIO(5056502364935393$144115188075855872), ir_112, CHAR) |
    f_MP_113(CONST_RATIO(5056502364935393$144115188075855872), CHAR, mp_113, CHAR) |
    f_ML_114(CONST_RATIO(5056502364935393$144115188075855872), CHAR, ml_114) |
    f_MR_115(CONST_RATIO(5056502364935393$144115188075855872), mr_115, CHAR) |
    f_D_116(CONST_RATIO(5056502364935393$144115188075855872), d_116) # h 
;


  mp_113 = f_IL_117(CONST_RATIO(3931825344300771$36893488147419103232), CHAR, il_117) |
    f_IR_118(CONST_RATIO(3931825344300771$36893488147419103232), ir_118, CHAR) |
    f_MP_119(CONST_RATIO(985954264173203$1125899906842624), CHAR, mp_119, CHAR) |
    f_ML_120(CONST_RATIO(3931825344300771$36893488147419103232), CHAR, ml_120) |
    f_MR_121(CONST_RATIO(3931825344300771$36893488147419103232), mr_121, CHAR) |
    f_D_122(CONST_RATIO(3931825344300771$36893488147419103232), d_122) # h 
;


  ml_114 = f_IL_117(CONST_RATIO(6919845182222927$288230376151711744), CHAR, il_117) |
    f_IR_118(CONST_RATIO(6919845182222927$288230376151711744), ir_118, CHAR) |
    f_MP_119(CONST_RATIO(6919845182222927$288230376151711744), CHAR, mp_119, CHAR) |
    f_ML_120(CONST_RATIO(6919845182222927$288230376151711744), CHAR, ml_120) |
    f_MR_121(CONST_RATIO(6919845182222927$288230376151711744), mr_121, CHAR) |
    f_D_122(CONST_RATIO(6919845182222927$288230376151711744), d_122) # h 
;


  mr_115 = f_IL_117(CONST_RATIO(6919845182222927$288230376151711744), CHAR, il_117) |
    f_IR_118(CONST_RATIO(6919845182222927$288230376151711744), ir_118, CHAR) |
    f_MP_119(CONST_RATIO(6919845182222927$288230376151711744), CHAR, mp_119, CHAR) |
    f_ML_120(CONST_RATIO(6919845182222927$288230376151711744), CHAR, ml_120) |
    f_MR_121(CONST_RATIO(6919845182222927$288230376151711744), mr_121, CHAR) |
    f_D_122(CONST_RATIO(6919845182222927$288230376151711744), d_122) # h 
;


  d_116 = f_IL_117(CONST_RATIO(6919845182222927$288230376151711744), CHAR, il_117) |
    f_IR_118(CONST_RATIO(6919845182222927$288230376151711744), ir_118, CHAR) |
    f_MP_119(CONST_RATIO(6919845182222927$288230376151711744), CHAR, mp_119, CHAR) |
    f_ML_120(CONST_RATIO(6919845182222927$288230376151711744), CHAR, ml_120) |
    f_MR_121(CONST_RATIO(6919845182222927$288230376151711744), mr_121, CHAR) |
    f_D_122(CONST_RATIO(6919845182222927$288230376151711744), d_122) # h 
;


  il_117 = f_IL_117(CONST_RATIO(6919845182222927$288230376151711744), CHAR, il_117) |
    f_IR_118(CONST_RATIO(6919845182222927$288230376151711744), ir_118, CHAR) |
    f_MP_119(CONST_RATIO(6919845182222927$288230376151711744), CHAR, mp_119, CHAR) |
    f_ML_120(CONST_RATIO(6919845182222927$288230376151711744), CHAR, ml_120) |
    f_MR_121(CONST_RATIO(6919845182222927$288230376151711744), mr_121, CHAR) |
    f_D_122(CONST_RATIO(6919845182222927$288230376151711744), d_122) # h 
;


  ir_118 = f_IR_118(CONST_RATIO(5056502364935393$144115188075855872), ir_118, CHAR) |
    f_MP_119(CONST_RATIO(5056502364935393$144115188075855872), CHAR, mp_119, CHAR) |
    f_ML_120(CONST_RATIO(5056502364935393$144115188075855872), CHAR, ml_120) |
    f_MR_121(CONST_RATIO(5056502364935393$144115188075855872), mr_121, CHAR) |
    f_D_122(CONST_RATIO(5056502364935393$144115188075855872), d_122) # h 
;


  mp_119 = f_IL_123(CONST_RATIO(8282857224389311$73786976294838206464), CHAR, il_123) |
    f_IR_124(CONST_RATIO(8282857224389311$73786976294838206464), ir_124, CHAR) |
    f_ML_125(CONST_RATIO(7797121839603345$9007199254740992), CHAR, ml_125) |
    f_D_126(CONST_RATIO(6181692104627111$4611686018427387904), d_126) # h 
;


  ml_120 = f_IL_123(CONST_RATIO(8046384138893273$144115188075855872), CHAR, il_123) |
    f_IR_124(CONST_RATIO(8046384138893273$144115188075855872), ir_124, CHAR) |
    f_ML_125(CONST_RATIO(8046384138893273$144115188075855872), CHAR, ml_125) |
    f_D_126(CONST_RATIO(8046384138893273$144115188075855872), d_126) # h 
;


  mr_121 = f_IL_123(CONST_RATIO(8046384138893273$144115188075855872), CHAR, il_123) |
    f_IR_124(CONST_RATIO(8046384138893273$144115188075855872), ir_124, CHAR) |
    f_ML_125(CONST_RATIO(8046384138893273$144115188075855872), CHAR, ml_125) |
    f_D_126(CONST_RATIO(8046384138893273$144115188075855872), d_126) # h 
;


  d_122 = f_IL_123(CONST_RATIO(8046384138893273$144115188075855872), CHAR, il_123) |
    f_IR_124(CONST_RATIO(8046384138893273$144115188075855872), ir_124, CHAR) |
    f_ML_125(CONST_RATIO(8046384138893273$144115188075855872), CHAR, ml_125) |
    f_D_126(CONST_RATIO(8046384138893273$144115188075855872), d_126) # h 
;


  il_123 = f_IL_123(CONST_RATIO(8046384138893273$144115188075855872), CHAR, il_123) |
    f_IR_124(CONST_RATIO(8046384138893273$144115188075855872), ir_124, CHAR) |
    f_ML_125(CONST_RATIO(8046384138893273$144115188075855872), CHAR, ml_125) |
    f_D_126(CONST_RATIO(8046384138893273$144115188075855872), d_126) # h 
;


  ir_124 = f_IR_124(CONST_RATIO(7321345163802901$72057594037927936), ir_124, CHAR) |
    f_ML_125(CONST_RATIO(7321345163802901$72057594037927936), CHAR, ml_125) |
    f_D_126(CONST_RATIO(7321345163802901$72057594037927936), d_126) # h 
;


  ml_125 = f_IL_127(CONST_RATIO(2264469954094969$18446744073709551616), CHAR, il_127) |
    f_ML_128(CONST_RATIO(8514401876804273$9007199254740992), CHAR, ml_128) |
    f_D_129(CONST_RATIO(2264469954094969$18446744073709551616), d_129) # h 
;


  d_126 = f_IL_127(CONST_RATIO(4492341323360557$144115188075855872), CHAR, il_127) |
    f_ML_128(CONST_RATIO(209546281385675$562949953421312), CHAR, ml_128) |
    f_D_129(CONST_RATIO(4492341323360557$144115188075855872), d_129) # h 
;


  il_127 = f_IL_127(CONST_RATIO(7321345163802901$72057594037927936), CHAR, il_127) |
    f_ML_128(CONST_RATIO(7321345163802901$72057594037927936), CHAR, ml_128) |
    f_D_129(CONST_RATIO(7321345163802901$72057594037927936), d_129) # h 
;


  ml_128 = f_IL_130(CONST_RATIO(74803177894991$562949953421312), CHAR, il_130) |
    f_ML_131(CONST_RATIO(6393720366637689$18014398509481984), CHAR, ml_131) |
    f_D_132(CONST_RATIO(4256520359554969$36893488147419103232), d_132) # h 
;


  d_129 = f_IL_130(CONST_RATIO(7321345163802901$72057594037927936), CHAR, il_130) |
    f_ML_131(CONST_RATIO(7321345163802901$72057594037927936), CHAR, ml_131) |
    f_D_132(CONST_RATIO(7321345163802901$72057594037927936), d_132) # h 
;


  il_130 = f_IL_130(CONST_RATIO(8502127047986841$9007199254740992), CHAR, il_130) |
    f_ML_131(CONST_RATIO(4771021776655561$9223372036854775808), CHAR, ml_131) |
    f_D_132(CONST_RATIO(4241955334421691$9444732965739290427392), d_132) # h 
;


  ml_131 = f_IL_133(CONST_RATIO(4256520359554969$36893488147419103232), CHAR, il_133) |
    f_ML_134(CONST_RATIO(2134751181217393$2251799813685248), CHAR, ml_134) |
    f_D_135(CONST_RATIO(4256520359554969$36893488147419103232), d_135) # h 
;


  d_132 = f_IL_133(CONST_RATIO(7321345163802901$72057594037927936), CHAR, il_133) |
    f_ML_134(CONST_RATIO(7321345163802901$72057594037927936), CHAR, ml_134) |
    f_D_135(CONST_RATIO(7321345163802901$72057594037927936), d_135) # h 
;


  il_133 = f_IL_133(CONST_RATIO(7321345163802901$72057594037927936), CHAR, il_133) |
    f_ML_134(CONST_RATIO(7321345163802901$72057594037927936), CHAR, ml_134) |
    f_D_135(CONST_RATIO(7321345163802901$72057594037927936), d_135) # h 
;


  ml_134 = f_IL_136(CONST_RATIO(4256520359554969$36893488147419103232), CHAR, il_136) |
    f_ML_137(CONST_RATIO(8404563337495611$9007199254740992), CHAR, ml_137) |
    f_D_138(CONST_RATIO(5138655651832799$18446744073709551616), d_138) # h 
;


  d_135 = f_IL_136(CONST_RATIO(7321345163802901$72057594037927936), CHAR, il_136) |
    f_ML_137(CONST_RATIO(7321345163802901$72057594037927936), CHAR, ml_137) |
    f_D_138(CONST_RATIO(7321345163802901$72057594037927936), d_138) # h 
;


  il_136 = f_IL_136(CONST_RATIO(7321345163802901$72057594037927936), CHAR, il_136) |
    f_ML_137(CONST_RATIO(7321345163802901$72057594037927936), CHAR, ml_137) |
    f_D_138(CONST_RATIO(7321345163802901$72057594037927936), d_138) # h 
;


  ml_137 = f_IL_139(CONST_RATIO(4312148470148809$36893488147419103232), CHAR, il_139) |
    f_ML_140(CONST_RATIO(4263347213618369$4503599627370496), CHAR, ml_140) |
    f_D_141(CONST_RATIO(4312148470148809$36893488147419103232), d_141) # h 
;


  d_138 = f_IL_139(CONST_RATIO(5223686804965673$72057594037927936), CHAR, il_139) |
    f_ML_140(CONST_RATIO(6315365432663377$36028797018963968), CHAR, ml_140) |
    f_D_141(CONST_RATIO(5223686804965673$72057594037927936), d_141) # h 
;


  il_139 = f_IL_139(CONST_RATIO(7321345163802901$72057594037927936), CHAR, il_139) |
    f_ML_140(CONST_RATIO(7321345163802901$72057594037927936), CHAR, ml_140) |
    f_D_141(CONST_RATIO(7321345163802901$72057594037927936), d_141) # h 
;


  ml_140 = f_E_143(CONST_RATIO(1095456780371855$1125899906842624), e_143) # h 
;


  d_141 = f_E_143(CONST_RATIO(532077976909867$2251799813685248), e_143) # h 
;


  e_143 = nil(EMPTY) 
;



}

instance count = Grammar(count);
instance probability = Grammar(Probability);
instance ambi = Grammar(Ambi);
instance ambiprob = Grammar(ambuni * Probability);
instance probpp = Grammar(propmax * Ambi);
instance enu = Grammar(enum);


