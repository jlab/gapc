/*
(multiple) covariance model for the following 2 consensus structure(s):
  default:  <<<<-<<<*-*-<<<<***---***---*>>>>*-<-<<<<*-******>>>>->**------------*---------*<<<-<<*-***-**--*>>>>->->>>>>>>-*    (as tree: 'P 1 (P 2 (P 3 (P 4 (P 6 (P 7 (P 8 (O 9 (O 11 (P 13 (P 14 (P 15 (P 16 (O 17 (O 18 (O 19 (O 23 (O 24 (O 25 (O 29 (E 30)))))))) (E 31)) (E 32)) (E 33)) (O 34 (P 36 (P 38 (P 39 (P 40 (P 41 (O 42 (O 44 (O 45 (O 46 (O 47 (O 48 (O 49 (E 50)))))))) (E 51)) (E 52)) (E 53)) (E 55)) (O 56 (O 57 (O 70 (O 80 (P 81 (P 82 (P 83 (P 85 (P 86 (O 87 (O 89 (O 90 (O 91 (O 93 (O 94 (O 97 (E 98)))))))) (E 99)) (E 100)) (E 101)) (E 103)) (E 105))))))))))) (E 106)) (E 107)) (E 108)) (E 109)) (E 110)) (E 111)) (O 113 (E 114))')
  varloop:  <<<<-<<<*-*-<<<<***---****--*>>>>*-<-<<<<*-******>>>>->*--<<<<<-<****->-->>>>>-*<<<-<<*-***-**--*>>>>->->>>>>>>-*    (as tree: 'P 1 (P 2 (P 3 (P 4 (P 6 (P 7 (P 8 (O 9 (O 11 (P 13 (P 14 (P 15 (P 16 (O 17 (O 18 (O 19 (O 23 (O 24 (O 25 (O 26 (O 29 (E 30))))))))) (E 31)) (E 32)) (E 33)) (O 34 (P 36 (P 38 (P 39 (P 40 (P 41 (O 42 (O 44 (O 45 (O 46 (O 47 (O 48 (O 49 (E 50)))))))) (E 51)) (E 52)) (E 53)) (E 55)) (O 56 (P 59 (P 60 (P 61 (P 62 (P 63 (P 65 (O 66 (O 67 (O 68 (O 69 (E 71))))) (E 74)) (E 75)) (E 76)) (E 77)) (E 78)) (O 80 (P 81 (P 82 (P 83 (P 85 (P 86 (O 87 (O 89 (O 90 (O 91 (O 93 (O 94 (O 97 (E 98)))))))) (E 99)) (E 100)) (E 101)) (E 103)) (E 105)))))))))) (E 106)) (E 107)) (E 108)) (E 109)) (E 110)) (E 111)) (O 113 (E 114))')
forming the unifying tree:
  [Pl 1 [Pl 2 [Pl 3 [Pl 4 [Pl 6 [Pl 7 [Pl 8 [Ol 9 [Ol 11 [Pl 13 [Pl 14 [Pl 15 [Pl 16 [Ol 17 [Ol 18 [Ol 19 [Ol 23 [Ol 24 [Ol 25 [Ol 26 [Ol 29 [El 30]],Ol 29 [El 30]]]]]]]] [El 31]] [El 32]] [El 33]] [Ol 34 [Pl 36 [Pl 38 [Pl 39 [Pl 40 [Pl 41 [Ol 42 [Ol 44 [Ol 45 [Ol 46 [Ol 47 [Ol 48 [Ol 49 [El 50]]]]]]]] [El 51]] [El 52]] [El 53]] [El 55]] [Ol 56 [Pl 59 [Pl 60 [Pl 61 [Pl 62 [Pl 63 [Pl 65 [Ol 66 [Ol 67 [Ol 68 [Ol 69 [El 71]]]]] [El 74]] [El 75]] [El 76]] [El 77]] [El 78]] [Ol 80 [Pl 81 [Pl 82 [Pl 83 [Pl 85 [Pl 86 [Ol 87 [Ol 89 [Ol 90 [Ol 91 [Ol 93 [Ol 94 [Ol 97 [El 98]]]]]]]] [El 99]] [El 100]] [El 101]] [El 103]] [El 105]]],Ol 57 [Ol 70 [Ol 80 [Pl 81 [Pl 82 [Pl 83 [Pl 85 [Pl 86 [Ol 87 [Ol 89 [Ol 90 [Ol 91 [Ol 93 [Ol 94 [Ol 97 [El 98]]]]]]]] [El 99]] [El 100]] [El 101]] [El 103]] [El 105]]]]]]]]]]] [El 106]] [El 107]] [El 108]] [El 109]] [El 110]] [El 111]] [Ol 113 [El 114]]]
*/

import "acmsearch_RF00005_probabilities.hh"
import isntimes

type Rope = extern
type ali = (Rope model, Rope seq, Rope cons)

signature sig_cm(alphabet, answer) {
  answer INS(alphabet, answer; int);
  answer NIL(void; int);
  answer MAT(alphabet, answer; int);
  answer DEL(answer; int);
  answer PK(alphabet, answer, alphabet, answer; int);
  answer Lr(alphabet, answer,           answer; int);
  answer lR(          answer, alphabet, answer; int);
  answer bg(          answer,           answer; int);
  choice [answer] h([answer]);
}
algebra alg_count auto count;

algebra alg_enum auto enum;

algebra alg_cyk implements sig_cm(alphabet = char, answer = float) {
  float NIL(void; int pos) {
    return getTransition_NIL(pos);
  }
  float INS(char a, float x; int pos) {
    return x + getTransition_INS(pos) + getEmission_INS(pos, a);
  }
  float MAT(char a, float x; int pos) {
    return x + getTransition_MAT(pos) + getEmission_MAT(pos, a);
  }
  float DEL(float x; int pos) {
    return x + getTransition_DEL(pos);
  }
  float PK (char a, float x, char b, float y; int pos) {
    return x + y + getTransition_PK(pos) + getEmission_PK(pos, a,b);
  }
  float Lr (char a, float x,         float y; int pos) {
    return x + y + getTransition_Lr(pos) + getEmission_Lr(pos, a);
  }
  float lR (        float x, char b, float y; int pos) {
    return x + y + getTransition_lR(pos) + getEmission_lR(pos, b);
  }
  float bg (        float x,         float y; int pos) {
    return x + y + getTransition_bg(pos);
  }
  choice [float] h([float] i) {
    return list(maximum(i));
  }
}
algebra alg_inside extends alg_cyk {
  choice [float] h([float] candidates) {
    return list(bitsum(candidates));
  }
}

algebra alg_align implements sig_cm(alphabet = char, answer = ali) {
  ali NIL(void; int pos) {
    ali res;
    return res;  
}
  ali INS(char a, ali x; int pos) {
    ali res;
    append(res.model, '-'); append(res.seq, a); append(res.cons, getConsensus_INS(pos));
    append(res.model, x.model); append(res.seq, x.seq); append(res.cons, x.cons);
    return res;
  }
  ali MAT(char a, ali x; int pos) {
    ali res;
    append(res.model, '*'); append(res.seq, a); append(res.cons, getConsensus_MAT(pos));
    append(res.model, x.model); append(res.seq, x.seq); append(res.cons, x.cons);
    return res;
  }
  ali DEL(ali x; int pos) {
    ali res;
    append(res.model, '*'); append(res.seq, '-'); append(res.cons, getConsensus_DEL(pos));
    append(res.model, x.model); append(res.seq, x.seq); append(res.cons, x.cons);
    return res;
  }
  ali PK (char a, ali x, char b, ali y; int pos) {
    ali res;
    append(res.model, '<'); append(res.seq, a  ); append(res.cons, getConsensus_PK(pos,0));
    append(res.model, x.model); append(res.seq, x.seq); append(res.cons, x.cons);
    append(res.model, '>'); append(res.seq, b  ); append(res.cons, getConsensus_PK(pos,1));
    append(res.model, y.model); append(res.seq, y.seq); append(res.cons, y.cons);
    return res;
  }
  ali Lr (char a, ali x,         ali y; int pos) {
    ali res;
    append(res.model, '<'); append(res.seq, a  ); append(res.cons, getConsensus_Lr(pos,0));
    append(res.model, x.model); append(res.seq, x.seq); append(res.cons, x.cons);
    append(res.model, '>'); append(res.seq, '-'); append(res.cons, getConsensus_Lr(pos,1));
    append(res.model, y.model); append(res.seq, y.seq); append(res.cons, y.cons);
    return res;
  }
  ali lR (        ali x, char b, ali y; int pos) {
    ali res;
    append(res.model, '<'); append(res.seq, '-'); append(res.cons, getConsensus_lR(pos,0));
    append(res.model, x.model); append(res.seq, x.seq); append(res.cons, x.cons);
    append(res.model, '>'); append(res.seq, b  ); append(res.cons, getConsensus_lR(pos,1));
    append(res.model, y.model); append(res.seq, y.seq); append(res.cons, y.cons);
    return res;
  }
  ali bg (        ali x,         ali y; int pos) {
    ali res;
    append(res.model, '<'); append(res.seq, '-'); append(res.cons, getConsensus_bg(pos,0));
    append(res.model, x.model); append(res.seq, x.seq); append(res.cons, x.cons);
    append(res.model, '>'); append(res.seq, '-'); append(res.cons, getConsensus_bg(pos,1));
    append(res.model, y.model); append(res.seq, y.seq); append(res.cons, y.cons);
    return res;
  }
  choice [ali] h([ali] i) {
    return i;
  }
}

grammar gra_build uses sig_cm(axiom = start) {
  start = a_1 # h;
  a_1 = INS(CHAR, a_1; 1) | PK(CHAR, a_2, CHAR, a_109; 1) | Lr(CHAR, a_2, a_109; 1) | lR(a_2, CHAR, a_109; 1) | bg(a_2, a_109; 1) # h;
  a_2 = INS(CHAR, a_2; 2) | PK(CHAR, a_3, CHAR, a_108; 2) | Lr(CHAR, a_3, a_108; 2) | lR(a_3, CHAR, a_108; 2) | bg(a_3, a_108; 2) # h;
  a_3 = INS(CHAR, a_3; 3) | PK(CHAR, a_4, CHAR, a_107; 3) | Lr(CHAR, a_4, a_107; 3) | lR(a_4, CHAR, a_107; 3) | bg(a_4, a_107; 3) # h;
  a_4 = INS(CHAR, a_4; 4) | PK(CHAR, a_5, CHAR, a_106; 4) | Lr(CHAR, a_5, a_106; 4) | lR(a_5, CHAR, a_106; 4) | bg(a_5, a_106; 4) # h;
  a_5 = INS(CHAR, a_5; 6) | PK(CHAR, a_6, CHAR, a_105; 6) | Lr(CHAR, a_6, a_105; 6) | lR(a_6, CHAR, a_105; 6) | bg(a_6, a_105; 6) # h;
  a_6 = INS(CHAR, a_6; 7) | PK(CHAR, a_7, CHAR, a_104; 7) | Lr(CHAR, a_7, a_104; 7) | lR(a_7, CHAR, a_104; 7) | bg(a_7, a_104; 7) # h;
  a_7 = INS(CHAR, a_7; 8) | PK(CHAR, a_8, CHAR, a_103; 8) | Lr(CHAR, a_8, a_103; 8) | lR(a_8, CHAR, a_103; 8) | bg(a_8, a_103; 8) # h;
  a_8 = INS(CHAR, a_8; 9) | MAT(CHAR, a_9; 9) | DEL(a_9; 9) # h;
  a_9 = INS(CHAR, a_9; 11) | MAT(CHAR, a_10; 11) | DEL(a_10; 11) # h;
  a_10 = INS(CHAR, a_10; 13) | PK(CHAR, a_11, CHAR, a_28; 13) | Lr(CHAR, a_11, a_28; 13) | lR(a_11, CHAR, a_28; 13) | bg(a_11, a_28; 13) # h;
  a_11 = INS(CHAR, a_11; 14) | PK(CHAR, a_12, CHAR, a_27; 14) | Lr(CHAR, a_12, a_27; 14) | lR(a_12, CHAR, a_27; 14) | bg(a_12, a_27; 14) # h;
  a_12 = INS(CHAR, a_12; 15) | PK(CHAR, a_13, CHAR, a_26; 15) | Lr(CHAR, a_13, a_26; 15) | lR(a_13, CHAR, a_26; 15) | bg(a_13, a_26; 15) # h;
  a_13 = INS(CHAR, a_13; 16) | PK(CHAR, a_14, CHAR, a_25; 16) | Lr(CHAR, a_14, a_25; 16) | lR(a_14, CHAR, a_25; 16) | bg(a_14, a_25; 16) # h;
  a_14 = INS(CHAR, a_14; 17) | MAT(CHAR, a_15; 17) | DEL(a_15; 17) # h;
  a_15 = INS(CHAR, a_15; 18) | MAT(CHAR, a_16; 18) | DEL(a_16; 18) # h;
  a_16 = INS(CHAR, a_16; 19) | MAT(CHAR, a_17; 19) | DEL(a_17; 19) # h;
  a_17 = INS(CHAR, a_17; 23) | MAT(CHAR, a_18; 23) | DEL(a_18; 23) # h;
  a_18 = INS(CHAR, a_18; 24) | MAT(CHAR, a_19; 24) | DEL(a_19; 24) # h;
  a_19 = INS(CHAR, a_19; 25) | MAT(CHAR, a_20; 25) | DEL(a_20; 25) | MAT(CHAR, a_23; 25) | DEL(a_23; 25) # h;
  a_20 = INS(CHAR, a_20; 26) | MAT(CHAR, a_21; 26) | DEL(a_21; 26) # h;
  a_21 = INS(CHAR, a_21; 29) | MAT(CHAR, a_22; 29) | DEL(a_22; 29) # h;
  a_22 = INS(CHAR, a_22; 30) | NIL(EMPTY; 30) # h;
  a_23 = INS(CHAR, a_23; 29) | MAT(CHAR, a_24; 29) | DEL(a_24; 29) # h;
  a_24 = INS(CHAR, a_24; 30) | NIL(EMPTY; 30) # h;
  a_25 = INS(CHAR, a_25; 31) | NIL(EMPTY; 31) # h;
  a_26 = INS(CHAR, a_26; 32) | NIL(EMPTY; 32) # h;
  a_27 = INS(CHAR, a_27; 33) | NIL(EMPTY; 33) # h;
  a_28 = INS(CHAR, a_28; 34) | MAT(CHAR, a_29; 34) | DEL(a_29; 34) # h;
  a_29 = INS(CHAR, a_29; 36) | PK(CHAR, a_30, CHAR, a_46; 36) | Lr(CHAR, a_30, a_46; 36) | lR(a_30, CHAR, a_46; 36) | bg(a_30, a_46; 36) # h;
  a_30 = INS(CHAR, a_30; 38) | PK(CHAR, a_31, CHAR, a_45; 38) | Lr(CHAR, a_31, a_45; 38) | lR(a_31, CHAR, a_45; 38) | bg(a_31, a_45; 38) # h;
  a_31 = INS(CHAR, a_31; 39) | PK(CHAR, a_32, CHAR, a_44; 39) | Lr(CHAR, a_32, a_44; 39) | lR(a_32, CHAR, a_44; 39) | bg(a_32, a_44; 39) # h;
  a_32 = INS(CHAR, a_32; 40) | PK(CHAR, a_33, CHAR, a_43; 40) | Lr(CHAR, a_33, a_43; 40) | lR(a_33, CHAR, a_43; 40) | bg(a_33, a_43; 40) # h;
  a_33 = INS(CHAR, a_33; 41) | PK(CHAR, a_34, CHAR, a_42; 41) | Lr(CHAR, a_34, a_42; 41) | lR(a_34, CHAR, a_42; 41) | bg(a_34, a_42; 41) # h;
  a_34 = INS(CHAR, a_34; 42) | MAT(CHAR, a_35; 42) | DEL(a_35; 42) # h;
  a_35 = INS(CHAR, a_35; 44) | MAT(CHAR, a_36; 44) | DEL(a_36; 44) # h;
  a_36 = INS(CHAR, a_36; 45) | MAT(CHAR, a_37; 45) | DEL(a_37; 45) # h;
  a_37 = INS(CHAR, a_37; 46) | MAT(CHAR, a_38; 46) | DEL(a_38; 46) # h;
  a_38 = INS(CHAR, a_38; 47) | MAT(CHAR, a_39; 47) | DEL(a_39; 47) # h;
  a_39 = INS(CHAR, a_39; 48) | MAT(CHAR, a_40; 48) | DEL(a_40; 48) # h;
  a_40 = INS(CHAR, a_40; 49) | MAT(CHAR, a_41; 49) | DEL(a_41; 49) # h;
  a_41 = INS(CHAR, a_41; 50) | NIL(EMPTY; 50) # h;
  a_42 = INS(CHAR, a_42; 51) | NIL(EMPTY; 51) # h;
  a_43 = INS(CHAR, a_43; 52) | NIL(EMPTY; 52) # h;
  a_44 = INS(CHAR, a_44; 53) | NIL(EMPTY; 53) # h;
  a_45 = INS(CHAR, a_45; 55) | NIL(EMPTY; 55) # h;
  a_46 = INS(CHAR, a_46; 56) | MAT(CHAR, a_47; 56) | DEL(a_47; 56) | MAT(CHAR, a_82; 56) | DEL(a_82; 56) # h;
  a_47 = INS(CHAR, a_47; 59) | PK(CHAR, a_48, CHAR, a_63; 59) | Lr(CHAR, a_48, a_63; 59) | lR(a_48, CHAR, a_63; 59) | bg(a_48, a_63; 59) # h;
  a_48 = INS(CHAR, a_48; 60) | PK(CHAR, a_49, CHAR, a_62; 60) | Lr(CHAR, a_49, a_62; 60) | lR(a_49, CHAR, a_62; 60) | bg(a_49, a_62; 60) # h;
  a_49 = INS(CHAR, a_49; 61) | PK(CHAR, a_50, CHAR, a_61; 61) | Lr(CHAR, a_50, a_61; 61) | lR(a_50, CHAR, a_61; 61) | bg(a_50, a_61; 61) # h;
  a_50 = INS(CHAR, a_50; 62) | PK(CHAR, a_51, CHAR, a_60; 62) | Lr(CHAR, a_51, a_60; 62) | lR(a_51, CHAR, a_60; 62) | bg(a_51, a_60; 62) # h;
  a_51 = INS(CHAR, a_51; 63) | PK(CHAR, a_52, CHAR, a_59; 63) | Lr(CHAR, a_52, a_59; 63) | lR(a_52, CHAR, a_59; 63) | bg(a_52, a_59; 63) # h;
  a_52 = INS(CHAR, a_52; 65) | PK(CHAR, a_53, CHAR, a_58; 65) | Lr(CHAR, a_53, a_58; 65) | lR(a_53, CHAR, a_58; 65) | bg(a_53, a_58; 65) # h;
  a_53 = INS(CHAR, a_53; 66) | MAT(CHAR, a_54; 66) | DEL(a_54; 66) # h;
  a_54 = INS(CHAR, a_54; 67) | MAT(CHAR, a_55; 67) | DEL(a_55; 67) # h;
  a_55 = INS(CHAR, a_55; 68) | MAT(CHAR, a_56; 68) | DEL(a_56; 68) # h;
  a_56 = INS(CHAR, a_56; 69) | MAT(CHAR, a_57; 69) | DEL(a_57; 69) # h;
  a_57 = INS(CHAR, a_57; 71) | NIL(EMPTY; 71) # h;
  a_58 = INS(CHAR, a_58; 74) | NIL(EMPTY; 74) # h;
  a_59 = INS(CHAR, a_59; 75) | NIL(EMPTY; 75) # h;
  a_60 = INS(CHAR, a_60; 76) | NIL(EMPTY; 76) # h;
  a_61 = INS(CHAR, a_61; 77) | NIL(EMPTY; 77) # h;
  a_62 = INS(CHAR, a_62; 78) | NIL(EMPTY; 78) # h;
  a_63 = INS(CHAR, a_63; 80) | MAT(CHAR, a_64; 80) | DEL(a_64; 80) # h;
  a_64 = INS(CHAR, a_64; 81) | PK(CHAR, a_65, CHAR, a_81; 81) | Lr(CHAR, a_65, a_81; 81) | lR(a_65, CHAR, a_81; 81) | bg(a_65, a_81; 81) # h;
  a_65 = INS(CHAR, a_65; 82) | PK(CHAR, a_66, CHAR, a_80; 82) | Lr(CHAR, a_66, a_80; 82) | lR(a_66, CHAR, a_80; 82) | bg(a_66, a_80; 82) # h;
  a_66 = INS(CHAR, a_66; 83) | PK(CHAR, a_67, CHAR, a_79; 83) | Lr(CHAR, a_67, a_79; 83) | lR(a_67, CHAR, a_79; 83) | bg(a_67, a_79; 83) # h;
  a_67 = INS(CHAR, a_67; 85) | PK(CHAR, a_68, CHAR, a_78; 85) | Lr(CHAR, a_68, a_78; 85) | lR(a_68, CHAR, a_78; 85) | bg(a_68, a_78; 85) # h;
  a_68 = INS(CHAR, a_68; 86) | PK(CHAR, a_69, CHAR, a_77; 86) | Lr(CHAR, a_69, a_77; 86) | lR(a_69, CHAR, a_77; 86) | bg(a_69, a_77; 86) # h;
  a_69 = INS(CHAR, a_69; 87) | MAT(CHAR, a_70; 87) | DEL(a_70; 87) # h;
  a_70 = INS(CHAR, a_70; 89) | MAT(CHAR, a_71; 89) | DEL(a_71; 89) # h;
  a_71 = INS(CHAR, a_71; 90) | MAT(CHAR, a_72; 90) | DEL(a_72; 90) # h;
  a_72 = INS(CHAR, a_72; 91) | MAT(CHAR, a_73; 91) | DEL(a_73; 91) # h;
  a_73 = INS(CHAR, a_73; 93) | MAT(CHAR, a_74; 93) | DEL(a_74; 93) # h;
  a_74 = INS(CHAR, a_74; 94) | MAT(CHAR, a_75; 94) | DEL(a_75; 94) # h;
  a_75 = INS(CHAR, a_75; 97) | MAT(CHAR, a_76; 97) | DEL(a_76; 97) # h;
  a_76 = INS(CHAR, a_76; 98) | NIL(EMPTY; 98) # h;
  a_77 = INS(CHAR, a_77; 99) | NIL(EMPTY; 99) # h;
  a_78 = INS(CHAR, a_78; 100) | NIL(EMPTY; 100) # h;
  a_79 = INS(CHAR, a_79; 101) | NIL(EMPTY; 101) # h;
  a_80 = INS(CHAR, a_80; 103) | NIL(EMPTY; 103) # h;
  a_81 = INS(CHAR, a_81; 105) | NIL(EMPTY; 105) # h;
  a_82 = INS(CHAR, a_82; 57) | MAT(CHAR, a_83; 57) | DEL(a_83; 57) # h;
  a_83 = INS(CHAR, a_83; 70) | MAT(CHAR, a_84; 70) | DEL(a_84; 70) # h;
  a_84 = INS(CHAR, a_84; 80) | MAT(CHAR, a_85; 80) | DEL(a_85; 80) # h;
  a_85 = INS(CHAR, a_85; 81) | PK(CHAR, a_86, CHAR, a_102; 81) | Lr(CHAR, a_86, a_102; 81) | lR(a_86, CHAR, a_102; 81) | bg(a_86, a_102; 81) # h;
  a_86 = INS(CHAR, a_86; 82) | PK(CHAR, a_87, CHAR, a_101; 82) | Lr(CHAR, a_87, a_101; 82) | lR(a_87, CHAR, a_101; 82) | bg(a_87, a_101; 82) # h;
  a_87 = INS(CHAR, a_87; 83) | PK(CHAR, a_88, CHAR, a_100; 83) | Lr(CHAR, a_88, a_100; 83) | lR(a_88, CHAR, a_100; 83) | bg(a_88, a_100; 83) # h;
  a_88 = INS(CHAR, a_88; 85) | PK(CHAR, a_89, CHAR, a_99; 85) | Lr(CHAR, a_89, a_99; 85) | lR(a_89, CHAR, a_99; 85) | bg(a_89, a_99; 85) # h;
  a_89 = INS(CHAR, a_89; 86) | PK(CHAR, a_90, CHAR, a_98; 86) | Lr(CHAR, a_90, a_98; 86) | lR(a_90, CHAR, a_98; 86) | bg(a_90, a_98; 86) # h;
  a_90 = INS(CHAR, a_90; 87) | MAT(CHAR, a_91; 87) | DEL(a_91; 87) # h;
  a_91 = INS(CHAR, a_91; 89) | MAT(CHAR, a_92; 89) | DEL(a_92; 89) # h;
  a_92 = INS(CHAR, a_92; 90) | MAT(CHAR, a_93; 90) | DEL(a_93; 90) # h;
  a_93 = INS(CHAR, a_93; 91) | MAT(CHAR, a_94; 91) | DEL(a_94; 91) # h;
  a_94 = INS(CHAR, a_94; 93) | MAT(CHAR, a_95; 93) | DEL(a_95; 93) # h;
  a_95 = INS(CHAR, a_95; 94) | MAT(CHAR, a_96; 94) | DEL(a_96; 94) # h;
  a_96 = INS(CHAR, a_96; 97) | MAT(CHAR, a_97; 97) | DEL(a_97; 97) # h;
  a_97 = INS(CHAR, a_97; 98) | NIL(EMPTY; 98) # h;
  a_98 = INS(CHAR, a_98; 99) | NIL(EMPTY; 99) # h;
  a_99 = INS(CHAR, a_99; 100) | NIL(EMPTY; 100) # h;
  a_100 = INS(CHAR, a_100; 101) | NIL(EMPTY; 101) # h;
  a_101 = INS(CHAR, a_101; 103) | NIL(EMPTY; 103) # h;
  a_102 = INS(CHAR, a_102; 105) | NIL(EMPTY; 105) # h;
  a_103 = INS(CHAR, a_103; 106) | NIL(EMPTY; 106) # h;
  a_104 = INS(CHAR, a_104; 107) | NIL(EMPTY; 107) # h;
  a_105 = INS(CHAR, a_105; 108) | NIL(EMPTY; 108) # h;
  a_106 = INS(CHAR, a_106; 109) | NIL(EMPTY; 109) # h;
  a_107 = INS(CHAR, a_107; 110) | NIL(EMPTY; 110) # h;
  a_108 = INS(CHAR, a_108; 111) | NIL(EMPTY; 111) # h;
  a_109 = INS(CHAR, a_109; 113) | MAT(CHAR, a_110; 113) | DEL(a_110; 113) # h;
  a_110 = INS(CHAR, a_110; 114) | NIL(EMPTY; 114) # h;
}

instance count = gra_build(alg_count);
instance train = gra_build(alg_enum);
instance cyk = gra_build(alg_cyk);
instance cykali = gra_build(alg_cyk * alg_align);
instance inside = gra_build(alg_inside);

