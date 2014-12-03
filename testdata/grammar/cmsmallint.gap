
import choener

signature Sig(alphabet, answer) {
answer f_IL_1(int,alphabet,answer);
answer f_IR_2(int,answer,alphabet);
answer f_ML_3(int,alphabet,answer);
answer f_D_4(int,answer);
answer f_IL_5(int,alphabet,answer);
answer f_ML_6(int,alphabet,answer);
answer f_D_7(int,answer);
answer f_IL_8(int,alphabet,answer);
answer f_ML_9(int,alphabet,answer);
answer f_D_10(int,answer);
answer f_IL_11(int,alphabet,answer);
answer f_ML_12(int,alphabet,answer);
answer f_D_13(int,answer);
answer f_IL_14(int,alphabet,answer);
answer f_ML_15(int,alphabet,answer);
answer f_D_16(int,answer);
answer f_IL_17(int,alphabet,answer);
answer f_MR_18(int,answer,alphabet);
answer f_D_19(int,answer);
answer f_IR_20(int,answer,alphabet);
answer f_MR_21(int,answer,alphabet);
answer f_D_22(int,answer);
answer f_IR_23(int,answer,alphabet);
answer f_MR_24(int,answer,alphabet);
answer f_D_25(int,answer);
answer f_IR_26(int,answer,alphabet);
answer f_MR_27(int,answer,alphabet);
answer f_D_28(int,answer);
answer f_IR_29(int,answer,alphabet);
answer f_MR_30(int,answer,alphabet);
answer f_D_31(int,answer);
answer f_IR_32(int,answer,alphabet);
answer f_MP_33(int,alphabet,answer,alphabet);
answer f_ML_34(int,alphabet,answer);
answer f_MR_35(int,answer,alphabet);
answer f_D_36(int,answer);
answer f_IL_37(int,alphabet,answer);
answer f_IR_38(int,answer,alphabet);
answer f_MP_39(int,alphabet,answer,alphabet);
answer f_ML_40(int,alphabet,answer);
answer f_MR_41(int,answer,alphabet);
answer f_D_42(int,answer);
answer f_IL_43(int,alphabet,answer);
answer f_IR_44(int,answer,alphabet);
answer f_MP_45(int,alphabet,answer,alphabet);
answer f_ML_46(int,alphabet,answer);
answer f_MR_47(int,answer,alphabet);
answer f_D_48(int,answer);
answer f_IL_49(int,alphabet,answer);
answer f_IR_50(int,answer,alphabet);
answer f_MP_51(int,alphabet,answer,alphabet);
answer f_ML_52(int,alphabet,answer);
answer f_MR_53(int,answer,alphabet);
answer f_D_54(int,answer);
answer f_IL_55(int,alphabet,answer);
answer f_IR_56(int,answer,alphabet);
answer f_MP_57(int,alphabet,answer,alphabet);
answer f_ML_58(int,alphabet,answer);
answer f_MR_59(int,answer,alphabet);
answer f_D_60(int,answer);
answer f_IL_61(int,alphabet,answer);
answer f_IR_62(int,answer,alphabet);
answer f_MP_63(int,alphabet,answer,alphabet);
answer f_ML_64(int,alphabet,answer);
answer f_MR_65(int,answer,alphabet);
answer f_D_66(int,answer);
answer f_IL_67(int,alphabet,answer);
answer f_IR_68(int,answer,alphabet);
answer f_ML_69(int,alphabet,answer);
answer f_D_70(int,answer);
answer f_IL_71(int,alphabet,answer);
answer f_ML_72(int,alphabet,answer);
answer f_D_73(int,answer);
answer f_IL_74(int,alphabet,answer);
answer f_ML_75(int,alphabet,answer);
answer f_D_76(int,answer);
answer f_IL_77(int,alphabet,answer);
answer f_ML_78(int,alphabet,answer);
answer f_D_79(int,answer);
answer f_E_81(int,answer);
answer nil(void);
choice [answer] h([answer]);
}

algebra Score implements Sig(alphabet = char, answer = int)
{
int f_IL_1(int p,  char b,  int s) { return p + s + lookup(b, 'a', 'c', 'g', 'u', 660, -612, -293, -76); }
int f_IR_2(int p,  int s,  char b) { return p + s + lookup(b, 'a', 'c', 'g', 'u', 660, -612, -293, -76); }
int f_ML_3(int p,  char b,  int s) { return p + s + lookup(b, 'a', 'c', 'g', 'u', 699, -183, -821, -106); }
int f_D_4(int p,  int s) { return p + s; }
int f_IL_5(int p,  char b,  int s) { return p + s + lookup(b, 'a', 'c', 'g', 'u', 660, -612, -293, -76); }
int f_ML_6(int p,  char b,  int s) { return p + s + lookup(b, 'a', 'c', 'g', 'u', 723, 13, -918, -302); }
int f_D_7(int p,  int s) { return p + s; }
int f_IL_8(int p,  char b,  int s) { return p + s + lookup(b, 'a', 'c', 'g', 'u', 660, -612, -293, -76); }
int f_ML_9(int p,  char b,  int s) { return p + s + lookup(b, 'a', 'c', 'g', 'u', 1347, -1128, -1255, -785); }
int f_D_10(int p,  int s) { return p + s; }
int f_IL_11(int p,  char b,  int s) { return p + s + lookup(b, 'a', 'c', 'g', 'u', 660, -612, -293, -76); }
int f_ML_12(int p,  char b,  int s) { return p + s + lookup(b, 'a', 'c', 'g', 'u', 1402, -1281, -1321, -874); }
int f_D_13(int p,  int s) { return p + s; }
int f_IL_14(int p,  char b,  int s) { return p + s + lookup(b, 'a', 'c', 'g', 'u', 660, -612, -293, -76); }
int f_ML_15(int p,  char b,  int s) { return p + s + lookup(b, 'a', 'c', 'g', 'u', 804, -100, -931, -329); }
int f_D_16(int p,  int s) { return p + s; }
int f_IL_17(int p,  char b,  int s) { return p + s + lookup(b, 'a', 'c', 'g', 'u', 660, -612, -293, -76); }
int f_MR_18(int p,  int s,  char b) { return p + s + lookup(b, 'a', 'c', 'g', 'u', 1002, -603, -975, -271); }
int f_D_19(int p,  int s) { return p + s; }
int f_IR_20(int p,  int s,  char b) { return p + s + lookup(b, 'a', 'c', 'g', 'u', 660, -612, -293, -76); }
int f_MR_21(int p,  int s,  char b) { return p + s + lookup(b, 'a', 'c', 'g', 'u', 930, -396, -949, -291); }
int f_D_22(int p,  int s) { return p + s; }
int f_IR_23(int p,  int s,  char b) { return p + s + lookup(b, 'a', 'c', 'g', 'u', 660, -612, -293, -76); }
int f_MR_24(int p,  int s,  char b) { return p + s + lookup(b, 'a', 'c', 'g', 'u', 353, 223, -880, 17); }
int f_D_25(int p,  int s) { return p + s; }
int f_IR_26(int p,  int s,  char b) { return p + s + lookup(b, 'a', 'c', 'g', 'u', 660, -612, -293, -76); }
int f_MR_27(int p,  int s,  char b) { return p + s + lookup(b, 'a', 'c', 'g', 'u', -55, 828, -1069, -347); }
int f_D_28(int p,  int s) { return p + s; }
int f_IR_29(int p,  int s,  char b) { return p + s + lookup(b, 'a', 'c', 'g', 'u', 660, -612, -293, -76); }
int f_MR_30(int p,  int s,  char b) { return p + s + lookup(b, 'a', 'c', 'g', 'u', 1314, -1044, -1220, -738); }
int f_D_31(int p,  int s) { return p + s; }
int f_IR_32(int p,  int s,  char b) { return p + s + lookup(b, 'a', 'c', 'g', 'u', 660, -612, -293, -76); }
int f_MP_33(int p, char a, int s,  char b) { return p + s + lookup2(a, b, 'a', 'c', 'g', 'u', 'a', 'c', 'g', 'u', -4369, -3201, -4864, 926, -4439, -4242, 475, -4402, -4126, 3301, -3897, -712, 293, -3855, -1337, -3489); }
int f_ML_34(int p,  char b,  int s) { return p + s + lookup(b, 'a', 'c', 'g', 'u', 660, -612, -293, -76); }
int f_MR_35(int p,  int s,  char b) { return p + s + lookup(b, 'a', 'c', 'g', 'u', 660, -612, -293, -76); }
int f_D_36(int p,  int s) { return p + s; }
int f_IL_37(int p,  char b,  int s) { return p + s + lookup(b, 'a', 'c', 'g', 'u', 660, -612, -293, -76); }
int f_IR_38(int p,  int s,  char b) { return p + s + lookup(b, 'a', 'c', 'g', 'u', 660, -612, -293, -76); }
int f_MP_39(int p, char a, int s,  char b) { return p + s + lookup2(a, b, 'a', 'c', 'g', 'u', 'a', 'c', 'g', 'u', -4343, -3187, -4843, 944, -4426, -4218, 477, -4392, -4112, 3292, -3883, -655, 297, -3836, -1340, -3477); }
int f_ML_40(int p,  char b,  int s) { return p + s + lookup(b, 'a', 'c', 'g', 'u', 660, -612, -293, -76); }
int f_MR_41(int p,  int s,  char b) { return p + s + lookup(b, 'a', 'c', 'g', 'u', 660, -612, -293, -76); }
int f_D_42(int p,  int s) { return p + s; }
int f_IL_43(int p,  char b,  int s) { return p + s + lookup(b, 'a', 'c', 'g', 'u', 660, -612, -293, -76); }
int f_IR_44(int p,  int s,  char b) { return p + s + lookup(b, 'a', 'c', 'g', 'u', 660, -612, -293, -76); }
int f_MP_45(int p, char a, int s,  char b) { return p + s + lookup2(a, b, 'a', 'c', 'g', 'u', 'a', 'c', 'g', 'u', -4027, -4013, -3870, 503, -3044, -4447, 3104, -4008, -4498, 674, -3961, -1233, 1318, -4251, -361, -3190); }
int f_ML_46(int p,  char b,  int s) { return p + s + lookup(b, 'a', 'c', 'g', 'u', 660, -612, -293, -76); }
int f_MR_47(int p,  int s,  char b) { return p + s + lookup(b, 'a', 'c', 'g', 'u', 660, -612, -293, -76); }
int f_D_48(int p,  int s) { return p + s; }
int f_IL_49(int p,  char b,  int s) { return p + s + lookup(b, 'a', 'c', 'g', 'u', 660, -612, -293, -76); }
int f_IR_50(int p,  int s,  char b) { return p + s + lookup(b, 'a', 'c', 'g', 'u', 660, -612, -293, -76); }
int f_MP_51(int p, char a, int s,  char b) { return p + s + lookup2(a, b, 'a', 'c', 'g', 'u', 'a', 'c', 'g', 'u', -3461, -3558, -3701, 853, -2705, -4283, 2529, -3722, -3880, 1010, -3865, -890, 2040, -3679, -218, -2737); }
int f_ML_52(int p,  char b,  int s) { return p + s + lookup(b, 'a', 'c', 'g', 'u', 660, -612, -293, -76); }
int f_MR_53(int p,  int s,  char b) { return p + s + lookup(b, 'a', 'c', 'g', 'u', 660, -612, -293, -76); }
int f_D_54(int p,  int s) { return p + s; }
int f_IL_55(int p,  char b,  int s) { return p + s + lookup(b, 'a', 'c', 'g', 'u', 660, -612, -293, -76); }
int f_IR_56(int p,  int s,  char b) { return p + s + lookup(b, 'a', 'c', 'g', 'u', 660, -612, -293, -76); }
int f_MP_57(int p, char a, int s,  char b) { return p + s + lookup2(a, b, 'a', 'c', 'g', 'u', 'a', 'c', 'g', 'u', -4258, -4112, -3968, 382, -3205, -4573, 3235, -4091, -4680, 553, -3999, -1268, 1051, -4465, -516, -3340); }
int f_ML_58(int p,  char b,  int s) { return p + s + lookup(b, 'a', 'c', 'g', 'u', 660, -612, -293, -76); }
int f_MR_59(int p,  int s,  char b) { return p + s + lookup(b, 'a', 'c', 'g', 'u', 660, -612, -293, -76); }
int f_D_60(int p,  int s) { return p + s; }
int f_IL_61(int p,  char b,  int s) { return p + s + lookup(b, 'a', 'c', 'g', 'u', 660, -612, -293, -76); }
int f_IR_62(int p,  int s,  char b) { return p + s + lookup(b, 'a', 'c', 'g', 'u', 660, -612, -293, -76); }
int f_MP_63(int p, char a, int s,  char b) { return p + s + lookup2(a, b, 'a', 'c', 'g', 'u', 'a', 'c', 'g', 'u', -3212, -3301, -3647, 855, -2615, -4347, 1670, -3561, -3539, 991, -3776, -686, 2719, -3452, -300, -2563); }
int f_ML_64(int p,  char b,  int s) { return p + s + lookup(b, 'a', 'c', 'g', 'u', 660, -612, -293, -76); }
int f_MR_65(int p,  int s,  char b) { return p + s + lookup(b, 'a', 'c', 'g', 'u', 660, -612, -293, -76); }
int f_D_66(int p,  int s) { return p + s; }
int f_IL_67(int p,  char b,  int s) { return p + s + lookup(b, 'a', 'c', 'g', 'u', 660, -612, -293, -76); }
int f_IR_68(int p,  int s,  char b) { return p + s + lookup(b, 'a', 'c', 'g', 'u', 660, -612, -293, -76); }
int f_ML_69(int p,  char b,  int s) { return p + s + lookup(b, 'a', 'c', 'g', 'u', -265, -913, -1099, 1118); }
int f_D_70(int p,  int s) { return p + s; }
int f_IL_71(int p,  char b,  int s) { return p + s + lookup(b, 'a', 'c', 'g', 'u', 660, -612, -293, -76); }
int f_ML_72(int p,  char b,  int s) { return p + s + lookup(b, 'a', 'c', 'g', 'u', 30, -740, -967, 902); }
int f_D_73(int p,  int s) { return p + s; }
int f_IL_74(int p,  char b,  int s) { return p + s + lookup(b, 'a', 'c', 'g', 'u', 660, -612, -293, -76); }
int f_ML_75(int p,  char b,  int s) { return p + s + lookup(b, 'a', 'c', 'g', 'u', -373, -1014, -1160, 1192); }
int f_D_76(int p,  int s) { return p + s; }
int f_IL_77(int p,  char b,  int s) { return p + s + lookup(b, 'a', 'c', 'g', 'u', 660, -612, -293, -76); }
int f_ML_78(int p,  char b,  int s) { return p + s + lookup(b, 'a', 'c', 'g', 'u', 339, 270, -887, -18); }
int f_D_79(int p,  int s) { return p + s; }
int f_E_81(int p,  int s) { return p + s; }
int nil(void) { return 0; }
  choice [int] h([int] i)
  {
    return list(maximum(i));
  }
}

algebra Pretty implements Sig(alphabet = char, answer = string)
{
string f_IL_1(int p,  char b,  string s) { string ret; append(ret, '.'); append(ret, s); return ret; }
string f_IR_2(int p,  string s,  char b) { string ret; append(ret, s); append(ret, '.'); return ret; }
string f_ML_3(int p,  char b,  string s) { string ret; append(ret, '.'); append(ret, s); return ret; }
string f_D_4(int p,  string s) { string ret; append(ret, s); return ret; }
string f_IL_5(int p,  char b,  string s) { string ret; append(ret, '.'); append(ret, s); return ret; }
string f_ML_6(int p,  char b,  string s) { string ret; append(ret, '.'); append(ret, s); return ret; }
string f_D_7(int p,  string s) { string ret; append(ret, s); return ret; }
string f_IL_8(int p,  char b,  string s) { string ret; append(ret, '.'); append(ret, s); return ret; }
string f_ML_9(int p,  char b,  string s) { string ret; append(ret, '.'); append(ret, s); return ret; }
string f_D_10(int p,  string s) { string ret; append(ret, s); return ret; }
string f_IL_11(int p,  char b,  string s) { string ret; append(ret, '.'); append(ret, s); return ret; }
string f_ML_12(int p,  char b,  string s) { string ret; append(ret, '.'); append(ret, s); return ret; }
string f_D_13(int p,  string s) { string ret; append(ret, s); return ret; }
string f_IL_14(int p,  char b,  string s) { string ret; append(ret, '.'); append(ret, s); return ret; }
string f_ML_15(int p,  char b,  string s) { string ret; append(ret, '.'); append(ret, s); return ret; }
string f_D_16(int p,  string s) { string ret; append(ret, s); return ret; }
string f_IL_17(int p,  char b,  string s) { string ret; append(ret, '.'); append(ret, s); return ret; }
string f_MR_18(int p,  string s,  char b) { string ret; append(ret, s); append(ret, '.'); return ret; }
string f_D_19(int p,  string s) { string ret; append(ret, s); return ret; }
string f_IR_20(int p,  string s,  char b) { string ret; append(ret, s); append(ret, '.'); return ret; }
string f_MR_21(int p,  string s,  char b) { string ret; append(ret, s); append(ret, '.'); return ret; }
string f_D_22(int p,  string s) { string ret; append(ret, s); return ret; }
string f_IR_23(int p,  string s,  char b) { string ret; append(ret, s); append(ret, '.'); return ret; }
string f_MR_24(int p,  string s,  char b) { string ret; append(ret, s); append(ret, '.'); return ret; }
string f_D_25(int p,  string s) { string ret; append(ret, s); return ret; }
string f_IR_26(int p,  string s,  char b) { string ret; append(ret, s); append(ret, '.'); return ret; }
string f_MR_27(int p,  string s,  char b) { string ret; append(ret, s); append(ret, '.'); return ret; }
string f_D_28(int p,  string s) { string ret; append(ret, s); return ret; }
string f_IR_29(int p,  string s,  char b) { string ret; append(ret, s); append(ret, '.'); return ret; }
string f_MR_30(int p,  string s,  char b) { string ret; append(ret, s); append(ret, '.'); return ret; }
string f_D_31(int p,  string s) { string ret; append(ret, s); return ret; }
string f_IR_32(int p,  string s,  char b) { string ret; append(ret, s); append(ret, '.'); return ret; }
string f_MP_33(int p,  char a, string s,  char b) { string ret; append(ret, '('); append(ret, s); append(ret, ')'); return ret; }
string f_ML_34(int p,  char b,  string s) { string ret; append(ret, '.'); append(ret, s); return ret; }
string f_MR_35(int p,  string s,  char b) { string ret; append(ret, s); append(ret, '.'); return ret; }
string f_D_36(int p,  string s) { string ret; append(ret, s); return ret; }
string f_IL_37(int p,  char b,  string s) { string ret; append(ret, '.'); append(ret, s); return ret; }
string f_IR_38(int p,  string s,  char b) { string ret; append(ret, s); append(ret, '.'); return ret; }
string f_MP_39(int p,  char a, string s,  char b) { string ret; append(ret, '('); append(ret, s); append(ret, ')'); return ret; }
string f_ML_40(int p,  char b,  string s) { string ret; append(ret, '.'); append(ret, s); return ret; }
string f_MR_41(int p,  string s,  char b) { string ret; append(ret, s); append(ret, '.'); return ret; }
string f_D_42(int p,  string s) { string ret; append(ret, s); return ret; }
string f_IL_43(int p,  char b,  string s) { string ret; append(ret, '.'); append(ret, s); return ret; }
string f_IR_44(int p,  string s,  char b) { string ret; append(ret, s); append(ret, '.'); return ret; }
string f_MP_45(int p,  char a, string s,  char b) { string ret; append(ret, '('); append(ret, s); append(ret, ')'); return ret; }
string f_ML_46(int p,  char b,  string s) { string ret; append(ret, '.'); append(ret, s); return ret; }
string f_MR_47(int p,  string s,  char b) { string ret; append(ret, s); append(ret, '.'); return ret; }
string f_D_48(int p,  string s) { string ret; append(ret, s); return ret; }
string f_IL_49(int p,  char b,  string s) { string ret; append(ret, '.'); append(ret, s); return ret; }
string f_IR_50(int p,  string s,  char b) { string ret; append(ret, s); append(ret, '.'); return ret; }
string f_MP_51(int p,  char a, string s,  char b) { string ret; append(ret, '('); append(ret, s); append(ret, ')'); return ret; }
string f_ML_52(int p,  char b,  string s) { string ret; append(ret, '.'); append(ret, s); return ret; }
string f_MR_53(int p,  string s,  char b) { string ret; append(ret, s); append(ret, '.'); return ret; }
string f_D_54(int p,  string s) { string ret; append(ret, s); return ret; }
string f_IL_55(int p,  char b,  string s) { string ret; append(ret, '.'); append(ret, s); return ret; }
string f_IR_56(int p,  string s,  char b) { string ret; append(ret, s); append(ret, '.'); return ret; }
string f_MP_57(int p,  char a, string s,  char b) { string ret; append(ret, '('); append(ret, s); append(ret, ')'); return ret; }
string f_ML_58(int p,  char b,  string s) { string ret; append(ret, '.'); append(ret, s); return ret; }
string f_MR_59(int p,  string s,  char b) { string ret; append(ret, s); append(ret, '.'); return ret; }
string f_D_60(int p,  string s) { string ret; append(ret, s); return ret; }
string f_IL_61(int p,  char b,  string s) { string ret; append(ret, '.'); append(ret, s); return ret; }
string f_IR_62(int p,  string s,  char b) { string ret; append(ret, s); append(ret, '.'); return ret; }
string f_MP_63(int p,  char a, string s,  char b) { string ret; append(ret, '('); append(ret, s); append(ret, ')'); return ret; }
string f_ML_64(int p,  char b,  string s) { string ret; append(ret, '.'); append(ret, s); return ret; }
string f_MR_65(int p,  string s,  char b) { string ret; append(ret, s); append(ret, '.'); return ret; }
string f_D_66(int p,  string s) { string ret; append(ret, s); return ret; }
string f_IL_67(int p,  char b,  string s) { string ret; append(ret, '.'); append(ret, s); return ret; }
string f_IR_68(int p,  string s,  char b) { string ret; append(ret, s); append(ret, '.'); return ret; }
string f_ML_69(int p,  char b,  string s) { string ret; append(ret, '.'); append(ret, s); return ret; }
string f_D_70(int p,  string s) { string ret; append(ret, s); return ret; }
string f_IL_71(int p,  char b,  string s) { string ret; append(ret, '.'); append(ret, s); return ret; }
string f_ML_72(int p,  char b,  string s) { string ret; append(ret, '.'); append(ret, s); return ret; }
string f_D_73(int p,  string s) { string ret; append(ret, s); return ret; }
string f_IL_74(int p,  char b,  string s) { string ret; append(ret, '.'); append(ret, s); return ret; }
string f_ML_75(int p,  char b,  string s) { string ret; append(ret, '.'); append(ret, s); return ret; }
string f_D_76(int p,  string s) { string ret; append(ret, s); return ret; }
string f_IL_77(int p,  char b,  string s) { string ret; append(ret, '.'); append(ret, s); return ret; }
string f_ML_78(int p,  char b,  string s) { string ret; append(ret, '.'); append(ret, s); return ret; }
string f_D_79(int p,  string s) { string ret; append(ret, s); return ret; }
string f_E_81(int p,  string s) { string ret; return ret; }
string nil(void) { string ret; return ret; }

  choice [string] h([string] i)
  {
    return i;
  }
}

algebra ppuni extends Pretty
{
  choice [string] h([string] i)
  {
    return unique(i);
  }
}

algebra count auto count ;

algebra enum auto enum ;


grammar Cm uses Sig(axiom = s_0) 
{
  s_0 = f_IL_1(CONST_INT(-6506), CHAR, il_1) |
    f_IR_2(CONST_INT(-6713), ir_2, CHAR) |
    f_ML_3(CONST_INT(-73), CHAR, ml_3) |
    f_D_4(CONST_INT(-5127), d_4) # h 
;


  il_1 = f_IL_1(CONST_INT(-1686), CHAR, il_1) |
    f_IR_2(CONST_INT(-2369), ir_2, CHAR) |
    f_ML_3(CONST_INT(-1117), CHAR, ml_3) |
    f_D_4(CONST_INT(-4855), d_4) # h 
;


  ir_2 = f_IR_2(CONST_INT(-1442), ir_2, CHAR) |
    f_ML_3(CONST_INT(-798), CHAR, ml_3) |
    f_D_4(CONST_INT(-4142), d_4) # h 
;


  ml_3 = f_IL_5(CONST_INT(-7559), CHAR, il_5) |
    f_ML_6(CONST_INT(-27), CHAR, ml_6) |
    f_D_7(CONST_INT(-6213), d_7) # h 
;


  d_4 = f_IL_5(CONST_INT(-6174), CHAR, il_5) |
    f_ML_6(CONST_INT(-1687), CHAR, ml_6) |
    f_D_7(CONST_INT(-566), d_7) # h 
;


  il_5 = f_IL_5(CONST_INT(-1442), CHAR, il_5) |
    f_ML_6(CONST_INT(-798), CHAR, ml_6) |
    f_D_7(CONST_INT(-4142), d_7) # h 
;


  ml_6 = f_IL_8(CONST_INT(-7559), CHAR, il_8) |
    f_ML_9(CONST_INT(-27), CHAR, ml_9) |
    f_D_10(CONST_INT(-6213), d_10) # h 
;


  d_7 = f_IL_8(CONST_INT(-6174), CHAR, il_8) |
    f_ML_9(CONST_INT(-1687), CHAR, ml_9) |
    f_D_10(CONST_INT(-566), d_10) # h 
;


  il_8 = f_IL_8(CONST_INT(-1442), CHAR, il_8) |
    f_ML_9(CONST_INT(-798), CHAR, ml_9) |
    f_D_10(CONST_INT(-4142), d_10) # h 
;


  ml_9 = f_IL_11(CONST_INT(-7559), CHAR, il_11) |
    f_ML_12(CONST_INT(-27), CHAR, ml_12) |
    f_D_13(CONST_INT(-6213), d_13) # h 
;


  d_10 = f_IL_11(CONST_INT(-6174), CHAR, il_11) |
    f_ML_12(CONST_INT(-1687), CHAR, ml_12) |
    f_D_13(CONST_INT(-566), d_13) # h 
;


  il_11 = f_IL_11(CONST_INT(-1442), CHAR, il_11) |
    f_ML_12(CONST_INT(-798), CHAR, ml_12) |
    f_D_13(CONST_INT(-4142), d_13) # h 
;


  ml_12 = f_IL_14(CONST_INT(-7559), CHAR, il_14) |
    f_ML_15(CONST_INT(-27), CHAR, ml_15) |
    f_D_16(CONST_INT(-6213), d_16) # h 
;


  d_13 = f_IL_14(CONST_INT(-6174), CHAR, il_14) |
    f_ML_15(CONST_INT(-1687), CHAR, ml_15) |
    f_D_16(CONST_INT(-566), d_16) # h 
;


  il_14 = f_IL_14(CONST_INT(-1442), CHAR, il_14) |
    f_ML_15(CONST_INT(-798), CHAR, ml_15) |
    f_D_16(CONST_INT(-4142), d_16) # h 
;


  ml_15 = f_IL_17(CONST_INT(-7979), CHAR, il_17) |
    f_MR_18(CONST_INT(-24), mr_18, CHAR) |
    f_D_19(CONST_INT(-6297), d_19) # h 
;


  d_16 = f_IL_17(CONST_INT(-5620), CHAR, il_17) |
    f_MR_18(CONST_INT(-734), mr_18, CHAR) |
    f_D_19(CONST_INT(-1403), d_19) # h 
;


  il_17 = f_IL_17(CONST_INT(-1925), CHAR, il_17) |
    f_MR_18(CONST_INT(-554), mr_18, CHAR) |
    f_D_19(CONST_INT(-4164), d_19) # h 
;


  mr_18 = f_IR_20(CONST_INT(-7979), ir_20, CHAR) |
    f_MR_21(CONST_INT(-24), mr_21, CHAR) |
    f_D_22(CONST_INT(-6297), d_22) # h 
;


  d_19 = f_IR_20(CONST_INT(-6390), ir_20, CHAR) |
    f_MR_21(CONST_INT(-1568), mr_21, CHAR) |
    f_D_22(CONST_INT(-620), d_22) # h 
;


  ir_20 = f_IR_20(CONST_INT(-1925), ir_20, CHAR) |
    f_MR_21(CONST_INT(-554), mr_21, CHAR) |
    f_D_22(CONST_INT(-4164), d_22) # h 
;


  mr_21 = f_IR_23(CONST_INT(-7979), ir_23, CHAR) |
    f_MR_24(CONST_INT(-24), mr_24, CHAR) |
    f_D_25(CONST_INT(-6297), d_25) # h 
;


  d_22 = f_IR_23(CONST_INT(-6390), ir_23, CHAR) |
    f_MR_24(CONST_INT(-1568), mr_24, CHAR) |
    f_D_25(CONST_INT(-620), d_25) # h 
;


  ir_23 = f_IR_23(CONST_INT(-1925), ir_23, CHAR) |
    f_MR_24(CONST_INT(-554), mr_24, CHAR) |
    f_D_25(CONST_INT(-4164), d_25) # h 
;


  mr_24 = f_IR_26(CONST_INT(-7979), ir_26, CHAR) |
    f_MR_27(CONST_INT(-24), mr_27, CHAR) |
    f_D_28(CONST_INT(-6297), d_28) # h 
;


  d_25 = f_IR_26(CONST_INT(-6390), ir_26, CHAR) |
    f_MR_27(CONST_INT(-1568), mr_27, CHAR) |
    f_D_28(CONST_INT(-620), d_28) # h 
;


  ir_26 = f_IR_26(CONST_INT(-1925), ir_26, CHAR) |
    f_MR_27(CONST_INT(-554), mr_27, CHAR) |
    f_D_28(CONST_INT(-4164), d_28) # h 
;


  mr_27 = f_IR_29(CONST_INT(-7979), ir_29, CHAR) |
    f_MR_30(CONST_INT(-24), mr_30, CHAR) |
    f_D_31(CONST_INT(-6297), d_31) # h 
;


  d_28 = f_IR_29(CONST_INT(-6390), ir_29, CHAR) |
    f_MR_30(CONST_INT(-1568), mr_30, CHAR) |
    f_D_31(CONST_INT(-620), d_31) # h 
;


  ir_29 = f_IR_29(CONST_INT(-1925), ir_29, CHAR) |
    f_MR_30(CONST_INT(-554), mr_30, CHAR) |
    f_D_31(CONST_INT(-4164), d_31) # h 
;


  mr_30 = f_IR_32(CONST_INT(-6746), ir_32, CHAR) |
    f_MP_33(CONST_INT(-50), CHAR, mp_33, CHAR) |
    f_ML_34(CONST_INT(-6562), CHAR, ml_34) |
    f_MR_35(CONST_INT(-6774), mr_35, CHAR) |
    f_D_36(CONST_INT(-7666), d_36) # h 
;


  d_31 = f_IR_32(CONST_INT(-5352), ir_32, CHAR) |
    f_MP_33(CONST_INT(-707), CHAR, mp_33, CHAR) |
    f_ML_34(CONST_INT(-2978), CHAR, ml_34) |
    f_MR_35(CONST_INT(-4409), mr_35, CHAR) |
    f_D_36(CONST_INT(-2404), d_36) # h 
;


  ir_32 = f_IR_32(CONST_INT(-2408), ir_32, CHAR) |
    f_MP_33(CONST_INT(-496), CHAR, mp_33, CHAR) |
    f_ML_34(CONST_INT(-5920), CHAR, ml_34) |
    f_MR_35(CONST_INT(-4086), mr_35, CHAR) |
    f_D_36(CONST_INT(-5193), d_36) # h 
;


  mp_33 = f_IL_37(CONST_INT(-8954), CHAR, il_37) |
    f_IR_38(CONST_INT(-8894), ir_38, CHAR) |
    f_MP_39(CONST_INT(-23), CHAR, mp_39, CHAR) |
    f_ML_40(CONST_INT(-7670), CHAR, ml_40) |
    f_MR_41(CONST_INT(-7950), mr_41, CHAR) |
    f_D_42(CONST_INT(-8345), d_42) # h 
;


  ml_34 = f_IL_37(CONST_INT(-6250), CHAR, il_37) |
    f_IR_38(CONST_INT(-6596), ir_38, CHAR) |
    f_MP_39(CONST_INT(-1310), CHAR, mp_39, CHAR) |
    f_ML_40(CONST_INT(-1004), CHAR, ml_40) |
    f_MR_41(CONST_INT(-6446), mr_41, CHAR) |
    f_D_42(CONST_INT(-3975), d_42) # h 
;


  mr_35 = f_IL_37(CONST_INT(-6988), CHAR, il_37) |
    f_IR_38(CONST_INT(-5717), ir_38, CHAR) |
    f_MP_39(CONST_INT(-1625), CHAR, mp_39, CHAR) |
    f_ML_40(CONST_INT(-5695), CHAR, ml_40) |
    f_MR_41(CONST_INT(-829), mr_41, CHAR) |
    f_D_42(CONST_INT(-3908), d_42) # h 
;


  d_36 = f_IL_37(CONST_INT(-9049), CHAR, il_37) |
    f_IR_38(CONST_INT(-7747), ir_38, CHAR) |
    f_MP_39(CONST_INT(-3544), CHAR, mp_39, CHAR) |
    f_ML_40(CONST_INT(-4226), CHAR, ml_40) |
    f_MR_41(CONST_INT(-4244), mr_41, CHAR) |
    f_D_42(CONST_INT(-319), d_42) # h 
;


  il_37 = f_IL_37(CONST_INT(-2579), CHAR, il_37) |
    f_IR_38(CONST_INT(-2842), ir_38, CHAR) |
    f_MP_39(CONST_INT(-760), CHAR, mp_39, CHAR) |
    f_ML_40(CONST_INT(-4497), CHAR, ml_40) |
    f_MR_41(CONST_INT(-5274), mr_41, CHAR) |
    f_D_42(CONST_INT(-4934), d_42) # h 
;


  ir_38 = f_IR_38(CONST_INT(-2408), ir_38, CHAR) |
    f_MP_39(CONST_INT(-496), CHAR, mp_39, CHAR) |
    f_ML_40(CONST_INT(-5920), CHAR, ml_40) |
    f_MR_41(CONST_INT(-4086), mr_41, CHAR) |
    f_D_42(CONST_INT(-5193), d_42) # h 
;


  mp_39 = f_IL_43(CONST_INT(-8954), CHAR, il_43) |
    f_IR_44(CONST_INT(-8894), ir_44, CHAR) |
    f_MP_45(CONST_INT(-23), CHAR, mp_45, CHAR) |
    f_ML_46(CONST_INT(-7670), CHAR, ml_46) |
    f_MR_47(CONST_INT(-7950), mr_47, CHAR) |
    f_D_48(CONST_INT(-8345), d_48) # h 
;


  ml_40 = f_IL_43(CONST_INT(-6250), CHAR, il_43) |
    f_IR_44(CONST_INT(-6596), ir_44, CHAR) |
    f_MP_45(CONST_INT(-1310), CHAR, mp_45, CHAR) |
    f_ML_46(CONST_INT(-1004), CHAR, ml_46) |
    f_MR_47(CONST_INT(-6446), mr_47, CHAR) |
    f_D_48(CONST_INT(-3975), d_48) # h 
;


  mr_41 = f_IL_43(CONST_INT(-6988), CHAR, il_43) |
    f_IR_44(CONST_INT(-5717), ir_44, CHAR) |
    f_MP_45(CONST_INT(-1625), CHAR, mp_45, CHAR) |
    f_ML_46(CONST_INT(-5695), CHAR, ml_46) |
    f_MR_47(CONST_INT(-829), mr_47, CHAR) |
    f_D_48(CONST_INT(-3908), d_48) # h 
;


  d_42 = f_IL_43(CONST_INT(-9049), CHAR, il_43) |
    f_IR_44(CONST_INT(-7747), ir_44, CHAR) |
    f_MP_45(CONST_INT(-3544), CHAR, mp_45, CHAR) |
    f_ML_46(CONST_INT(-4226), CHAR, ml_46) |
    f_MR_47(CONST_INT(-4244), mr_47, CHAR) |
    f_D_48(CONST_INT(-319), d_48) # h 
;


  il_43 = f_IL_43(CONST_INT(-2579), CHAR, il_43) |
    f_IR_44(CONST_INT(-2842), ir_44, CHAR) |
    f_MP_45(CONST_INT(-760), CHAR, mp_45, CHAR) |
    f_ML_46(CONST_INT(-4497), CHAR, ml_46) |
    f_MR_47(CONST_INT(-5274), mr_47, CHAR) |
    f_D_48(CONST_INT(-4934), d_48) # h 
;


  ir_44 = f_IR_44(CONST_INT(-2408), ir_44, CHAR) |
    f_MP_45(CONST_INT(-496), CHAR, mp_45, CHAR) |
    f_ML_46(CONST_INT(-5920), CHAR, ml_46) |
    f_MR_47(CONST_INT(-4086), mr_47, CHAR) |
    f_D_48(CONST_INT(-5193), d_48) # h 
;


  mp_45 = f_IL_49(CONST_INT(-8954), CHAR, il_49) |
    f_IR_50(CONST_INT(-8894), ir_50, CHAR) |
    f_MP_51(CONST_INT(-23), CHAR, mp_51, CHAR) |
    f_ML_52(CONST_INT(-7670), CHAR, ml_52) |
    f_MR_53(CONST_INT(-7950), mr_53, CHAR) |
    f_D_54(CONST_INT(-8345), d_54) # h 
;


  ml_46 = f_IL_49(CONST_INT(-6250), CHAR, il_49) |
    f_IR_50(CONST_INT(-6596), ir_50, CHAR) |
    f_MP_51(CONST_INT(-1310), CHAR, mp_51, CHAR) |
    f_ML_52(CONST_INT(-1004), CHAR, ml_52) |
    f_MR_53(CONST_INT(-6446), mr_53, CHAR) |
    f_D_54(CONST_INT(-3975), d_54) # h 
;


  mr_47 = f_IL_49(CONST_INT(-6988), CHAR, il_49) |
    f_IR_50(CONST_INT(-5717), ir_50, CHAR) |
    f_MP_51(CONST_INT(-1625), CHAR, mp_51, CHAR) |
    f_ML_52(CONST_INT(-5695), CHAR, ml_52) |
    f_MR_53(CONST_INT(-829), mr_53, CHAR) |
    f_D_54(CONST_INT(-3908), d_54) # h 
;


  d_48 = f_IL_49(CONST_INT(-9049), CHAR, il_49) |
    f_IR_50(CONST_INT(-7747), ir_50, CHAR) |
    f_MP_51(CONST_INT(-3544), CHAR, mp_51, CHAR) |
    f_ML_52(CONST_INT(-4226), CHAR, ml_52) |
    f_MR_53(CONST_INT(-4244), mr_53, CHAR) |
    f_D_54(CONST_INT(-319), d_54) # h 
;


  il_49 = f_IL_49(CONST_INT(-2579), CHAR, il_49) |
    f_IR_50(CONST_INT(-2842), ir_50, CHAR) |
    f_MP_51(CONST_INT(-760), CHAR, mp_51, CHAR) |
    f_ML_52(CONST_INT(-4497), CHAR, ml_52) |
    f_MR_53(CONST_INT(-5274), mr_53, CHAR) |
    f_D_54(CONST_INT(-4934), d_54) # h 
;


  ir_50 = f_IR_50(CONST_INT(-2408), ir_50, CHAR) |
    f_MP_51(CONST_INT(-496), CHAR, mp_51, CHAR) |
    f_ML_52(CONST_INT(-5920), CHAR, ml_52) |
    f_MR_53(CONST_INT(-4086), mr_53, CHAR) |
    f_D_54(CONST_INT(-5193), d_54) # h 
;


  mp_51 = f_IL_55(CONST_INT(-8954), CHAR, il_55) |
    f_IR_56(CONST_INT(-8894), ir_56, CHAR) |
    f_MP_57(CONST_INT(-23), CHAR, mp_57, CHAR) |
    f_ML_58(CONST_INT(-7670), CHAR, ml_58) |
    f_MR_59(CONST_INT(-7950), mr_59, CHAR) |
    f_D_60(CONST_INT(-8345), d_60) # h 
;


  ml_52 = f_IL_55(CONST_INT(-6250), CHAR, il_55) |
    f_IR_56(CONST_INT(-6596), ir_56, CHAR) |
    f_MP_57(CONST_INT(-1310), CHAR, mp_57, CHAR) |
    f_ML_58(CONST_INT(-1004), CHAR, ml_58) |
    f_MR_59(CONST_INT(-6446), mr_59, CHAR) |
    f_D_60(CONST_INT(-3975), d_60) # h 
;


  mr_53 = f_IL_55(CONST_INT(-6988), CHAR, il_55) |
    f_IR_56(CONST_INT(-5717), ir_56, CHAR) |
    f_MP_57(CONST_INT(-1625), CHAR, mp_57, CHAR) |
    f_ML_58(CONST_INT(-5695), CHAR, ml_58) |
    f_MR_59(CONST_INT(-829), mr_59, CHAR) |
    f_D_60(CONST_INT(-3908), d_60) # h 
;


  d_54 = f_IL_55(CONST_INT(-9049), CHAR, il_55) |
    f_IR_56(CONST_INT(-7747), ir_56, CHAR) |
    f_MP_57(CONST_INT(-3544), CHAR, mp_57, CHAR) |
    f_ML_58(CONST_INT(-4226), CHAR, ml_58) |
    f_MR_59(CONST_INT(-4244), mr_59, CHAR) |
    f_D_60(CONST_INT(-319), d_60) # h 
;


  il_55 = f_IL_55(CONST_INT(-2579), CHAR, il_55) |
    f_IR_56(CONST_INT(-2842), ir_56, CHAR) |
    f_MP_57(CONST_INT(-760), CHAR, mp_57, CHAR) |
    f_ML_58(CONST_INT(-4497), CHAR, ml_58) |
    f_MR_59(CONST_INT(-5274), mr_59, CHAR) |
    f_D_60(CONST_INT(-4934), d_60) # h 
;


  ir_56 = f_IR_56(CONST_INT(-2408), ir_56, CHAR) |
    f_MP_57(CONST_INT(-496), CHAR, mp_57, CHAR) |
    f_ML_58(CONST_INT(-5920), CHAR, ml_58) |
    f_MR_59(CONST_INT(-4086), mr_59, CHAR) |
    f_D_60(CONST_INT(-5193), d_60) # h 
;


  mp_57 = f_IL_61(CONST_INT(-8954), CHAR, il_61) |
    f_IR_62(CONST_INT(-8894), ir_62, CHAR) |
    f_MP_63(CONST_INT(-23), CHAR, mp_63, CHAR) |
    f_ML_64(CONST_INT(-7670), CHAR, ml_64) |
    f_MR_65(CONST_INT(-7950), mr_65, CHAR) |
    f_D_66(CONST_INT(-8345), d_66) # h 
;


  ml_58 = f_IL_61(CONST_INT(-6250), CHAR, il_61) |
    f_IR_62(CONST_INT(-6596), ir_62, CHAR) |
    f_MP_63(CONST_INT(-1310), CHAR, mp_63, CHAR) |
    f_ML_64(CONST_INT(-1004), CHAR, ml_64) |
    f_MR_65(CONST_INT(-6446), mr_65, CHAR) |
    f_D_66(CONST_INT(-3975), d_66) # h 
;


  mr_59 = f_IL_61(CONST_INT(-6988), CHAR, il_61) |
    f_IR_62(CONST_INT(-5717), ir_62, CHAR) |
    f_MP_63(CONST_INT(-1625), CHAR, mp_63, CHAR) |
    f_ML_64(CONST_INT(-5695), CHAR, ml_64) |
    f_MR_65(CONST_INT(-829), mr_65, CHAR) |
    f_D_66(CONST_INT(-3908), d_66) # h 
;


  d_60 = f_IL_61(CONST_INT(-9049), CHAR, il_61) |
    f_IR_62(CONST_INT(-7747), ir_62, CHAR) |
    f_MP_63(CONST_INT(-3544), CHAR, mp_63, CHAR) |
    f_ML_64(CONST_INT(-4226), CHAR, ml_64) |
    f_MR_65(CONST_INT(-4244), mr_65, CHAR) |
    f_D_66(CONST_INT(-319), d_66) # h 
;


  il_61 = f_IL_61(CONST_INT(-2579), CHAR, il_61) |
    f_IR_62(CONST_INT(-2842), ir_62, CHAR) |
    f_MP_63(CONST_INT(-760), CHAR, mp_63, CHAR) |
    f_ML_64(CONST_INT(-4497), CHAR, ml_64) |
    f_MR_65(CONST_INT(-5274), mr_65, CHAR) |
    f_D_66(CONST_INT(-4934), d_66) # h 
;


  ir_62 = f_IR_62(CONST_INT(-2408), ir_62, CHAR) |
    f_MP_63(CONST_INT(-496), CHAR, mp_63, CHAR) |
    f_ML_64(CONST_INT(-5920), CHAR, ml_64) |
    f_MR_65(CONST_INT(-4086), mr_65, CHAR) |
    f_D_66(CONST_INT(-5193), d_66) # h 
;


  mp_63 = f_IL_67(CONST_INT(-6506), CHAR, il_67) |
    f_IR_68(CONST_INT(-6713), ir_68, CHAR) |
    f_ML_69(CONST_INT(-73), CHAR, ml_69) |
    f_D_70(CONST_INT(-5127), d_70) # h 
;


  ml_64 = f_IL_67(CONST_INT(-3758), CHAR, il_67) |
    f_IR_68(CONST_INT(-3940), ir_68, CHAR) |
    f_ML_69(CONST_INT(-507), CHAR, ml_69) |
    f_D_70(CONST_INT(-2670), d_70) # h 
;


  mr_65 = f_IL_67(CONST_INT(-4809), CHAR, il_67) |
    f_IR_68(CONST_INT(-3838), ir_68, CHAR) |
    f_ML_69(CONST_INT(-1706), CHAR, ml_69) |
    f_D_70(CONST_INT(-766), d_70) # h 
;


  d_66 = f_IL_67(CONST_INT(-4568), CHAR, il_67) |
    f_IR_68(CONST_INT(-4250), ir_68, CHAR) |
    f_ML_69(CONST_INT(-2265), CHAR, ml_69) |
    f_D_70(CONST_INT(-520), d_70) # h 
;


  il_67 = f_IL_67(CONST_INT(-1686), CHAR, il_67) |
    f_IR_68(CONST_INT(-2369), ir_68, CHAR) |
    f_ML_69(CONST_INT(-1117), CHAR, ml_69) |
    f_D_70(CONST_INT(-4855), d_70) # h 
;


  ir_68 = f_IR_68(CONST_INT(-1442), ir_68, CHAR) |
    f_ML_69(CONST_INT(-798), CHAR, ml_69) |
    f_D_70(CONST_INT(-4142), d_70) # h 
;


  ml_69 = f_IL_71(CONST_INT(-7559), CHAR, il_71) |
    f_ML_72(CONST_INT(-27), CHAR, ml_72) |
    f_D_73(CONST_INT(-6213), d_73) # h 
;


  d_70 = f_IL_71(CONST_INT(-6174), CHAR, il_71) |
    f_ML_72(CONST_INT(-1687), CHAR, ml_72) |
    f_D_73(CONST_INT(-566), d_73) # h 
;


  il_71 = f_IL_71(CONST_INT(-1442), CHAR, il_71) |
    f_ML_72(CONST_INT(-798), CHAR, ml_72) |
    f_D_73(CONST_INT(-4142), d_73) # h 
;


  ml_72 = f_IL_74(CONST_INT(-7559), CHAR, il_74) |
    f_ML_75(CONST_INT(-27), CHAR, ml_75) |
    f_D_76(CONST_INT(-6213), d_76) # h 
;


  d_73 = f_IL_74(CONST_INT(-6174), CHAR, il_74) |
    f_ML_75(CONST_INT(-1687), CHAR, ml_75) |
    f_D_76(CONST_INT(-566), d_76) # h 
;


  il_74 = f_IL_74(CONST_INT(-1442), CHAR, il_74) |
    f_ML_75(CONST_INT(-798), CHAR, ml_75) |
    f_D_76(CONST_INT(-4142), d_76) # h 
;


  ml_75 = f_IL_77(CONST_INT(-7559), CHAR, il_77) |
    f_ML_78(CONST_INT(-27), CHAR, ml_78) |
    f_D_79(CONST_INT(-6213), d_79) # h 
;


  d_76 = f_IL_77(CONST_INT(-6174), CHAR, il_77) |
    f_ML_78(CONST_INT(-1687), CHAR, ml_78) |
    f_D_79(CONST_INT(-566), d_79) # h 
;


  il_77 = f_IL_77(CONST_INT(-1442), CHAR, il_77) |
    f_ML_78(CONST_INT(-798), CHAR, ml_78) |
    f_D_79(CONST_INT(-4142), d_79) # h 
;


  ml_78 = 
    f_E_81(CONST_INT(-6), e_81) # h 
;


  d_79 = 
    f_E_81(CONST_INT(-4), e_81) # h 
;


  e_81 = nil(EMPTY) 
;



}

instance score = Cm ( Score ) ;

instance pretty = Cm ( Pretty ) ;

instance scorepp = Cm ( Score * Pretty ) ;

instance ppscore = Cm ( ppuni * Score ) ;

instance count = Cm ( count ) ;

instance enum = Cm ( enum ) ;


