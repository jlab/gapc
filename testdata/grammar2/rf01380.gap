//import bitsum
//import inside

type alignment = (string sequence, string model)
type Rope = extern

signature Algebra(alphabet, answer) {
	answer skipl(alphabet, answer);
	answer skipr(answer, alphabet);
  answer window(Subsequence, answer, Subsequence);
  answer root__global(answer);
  answer root__local(answer);
  answer nil(void);
  answer local__MP_2(answer);
  answer local__MP_3(answer);
  answer local__MP_4(answer);
  answer local__MR_5(answer);
  answer local__MP_6(answer);
  answer local__MP_7(answer);
  answer local__MP_8(answer);
  answer local__ML_9(answer);
  answer local__ML_10(answer);
  answer local__ML_11(answer);
  answer local__ML_12(answer);
  answer MP_1__IL_1(alphabet, answer, alphabet);
  answer MP_1__IR_1(alphabet, answer, alphabet);
  answer MP_1__MP_2(alphabet, answer, alphabet);
  answer MP_1__ML_2(alphabet, answer, alphabet);
  answer MP_1__MR_2(alphabet, answer, alphabet);
  answer MP_1__D_2(alphabet, answer, alphabet);
  answer MP_1__EL(alphabet, Subsequence, alphabet);
  answer IL_1__IL_1(alphabet, answer);
  answer IL_1__IR_1(alphabet, answer);
  answer IL_1__MP_2(alphabet, answer);
  answer IL_1__ML_2(alphabet, answer);
  answer IL_1__MR_2(alphabet, answer);
  answer IL_1__D_2(alphabet, answer);
  answer IR_1__IR_1(answer, alphabet);
  answer IR_1__MP_2(answer, alphabet);
  answer IR_1__ML_2(answer, alphabet);
  answer IR_1__MR_2(answer, alphabet);
  answer IR_1__D_2(answer, alphabet);
  answer MP_2__IL_2(alphabet, answer, alphabet);
  answer MP_2__IR_2(alphabet, answer, alphabet);
  answer MP_2__MP_3(alphabet, answer, alphabet);
  answer MP_2__ML_3(alphabet, answer, alphabet);
  answer MP_2__MR_3(alphabet, answer, alphabet);
  answer MP_2__D_3(alphabet, answer, alphabet);
  answer MP_2__EL(alphabet, Subsequence, alphabet);
  answer ML_2__IL_2(alphabet, answer);
  answer ML_2__IR_2(alphabet, answer);
  answer ML_2__MP_3(alphabet, answer);
  answer ML_2__ML_3(alphabet, answer);
  answer ML_2__MR_3(alphabet, answer);
  answer ML_2__D_3(alphabet, answer);
  answer MR_2__IL_2(answer, alphabet);
  answer MR_2__IR_2(answer, alphabet);
  answer MR_2__MP_3(answer, alphabet);
  answer MR_2__ML_3(answer, alphabet);
  answer MR_2__MR_3(answer, alphabet);
  answer MR_2__D_3(answer, alphabet);
  answer D_2__IL_2(answer);
  answer D_2__IR_2(answer);
  answer D_2__MP_3(answer);
  answer D_2__ML_3(answer);
  answer D_2__MR_3(answer);
  answer D_2__D_3(answer);
  answer IL_2__IL_2(alphabet, answer);
  answer IL_2__IR_2(alphabet, answer);
  answer IL_2__MP_3(alphabet, answer);
  answer IL_2__ML_3(alphabet, answer);
  answer IL_2__MR_3(alphabet, answer);
  answer IL_2__D_3(alphabet, answer);
  answer IR_2__IR_2(answer, alphabet);
  answer IR_2__MP_3(answer, alphabet);
  answer IR_2__ML_3(answer, alphabet);
  answer IR_2__MR_3(answer, alphabet);
  answer IR_2__D_3(answer, alphabet);
  answer MP_3__IL_3(alphabet, answer, alphabet);
  answer MP_3__IR_3(alphabet, answer, alphabet);
  answer MP_3__MP_4(alphabet, answer, alphabet);
  answer MP_3__ML_4(alphabet, answer, alphabet);
  answer MP_3__MR_4(alphabet, answer, alphabet);
  answer MP_3__D_4(alphabet, answer, alphabet);
  answer MP_3__EL(alphabet, Subsequence, alphabet);
  answer ML_3__IL_3(alphabet, answer);
  answer ML_3__IR_3(alphabet, answer);
  answer ML_3__MP_4(alphabet, answer);
  answer ML_3__ML_4(alphabet, answer);
  answer ML_3__MR_4(alphabet, answer);
  answer ML_3__D_4(alphabet, answer);
  answer MR_3__IL_3(answer, alphabet);
  answer MR_3__IR_3(answer, alphabet);
  answer MR_3__MP_4(answer, alphabet);
  answer MR_3__ML_4(answer, alphabet);
  answer MR_3__MR_4(answer, alphabet);
  answer MR_3__D_4(answer, alphabet);
  answer D_3__IL_3(answer);
  answer D_3__IR_3(answer);
  answer D_3__MP_4(answer);
  answer D_3__ML_4(answer);
  answer D_3__MR_4(answer);
  answer D_3__D_4(answer);
  answer IL_3__IL_3(alphabet, answer);
  answer IL_3__IR_3(alphabet, answer);
  answer IL_3__MP_4(alphabet, answer);
  answer IL_3__ML_4(alphabet, answer);
  answer IL_3__MR_4(alphabet, answer);
  answer IL_3__D_4(alphabet, answer);
  answer IR_3__IR_3(answer, alphabet);
  answer IR_3__MP_4(answer, alphabet);
  answer IR_3__ML_4(answer, alphabet);
  answer IR_3__MR_4(answer, alphabet);
  answer IR_3__D_4(answer, alphabet);
  answer MP_4__IL_4(alphabet, answer, alphabet);
  answer MP_4__IR_4(alphabet, answer, alphabet);
  answer MP_4__MR_5(alphabet, answer, alphabet);
  answer MP_4__D_5(alphabet, answer, alphabet);
  answer MP_4__EL(alphabet, Subsequence, alphabet);
  answer ML_4__IL_4(alphabet, answer);
  answer ML_4__IR_4(alphabet, answer);
  answer ML_4__MR_5(alphabet, answer);
  answer ML_4__D_5(alphabet, answer);
  answer MR_4__IL_4(answer, alphabet);
  answer MR_4__IR_4(answer, alphabet);
  answer MR_4__MR_5(answer, alphabet);
  answer MR_4__D_5(answer, alphabet);
  answer D_4__IL_4(answer);
  answer D_4__IR_4(answer);
  answer D_4__MR_5(answer);
  answer D_4__D_5(answer);
  answer IL_4__IL_4(alphabet, answer);
  answer IL_4__IR_4(alphabet, answer);
  answer IL_4__MR_5(alphabet, answer);
  answer IL_4__D_5(alphabet, answer);
  answer IR_4__IR_4(answer, alphabet);
  answer IR_4__MR_5(answer, alphabet);
  answer IR_4__D_5(answer, alphabet);
  answer MR_5__IR_5(answer, alphabet);
  answer MR_5__MP_6(answer, alphabet);
  answer MR_5__ML_6(answer, alphabet);
  answer MR_5__MR_6(answer, alphabet);
  answer MR_5__D_6(answer, alphabet);
  answer MR_5__EL(Subsequence, alphabet);
  answer D_5__IR_5(answer);
  answer D_5__MP_6(answer);
  answer D_5__ML_6(answer);
  answer D_5__MR_6(answer);
  answer D_5__D_6(answer);
  answer IR_5__IR_5(answer, alphabet);
  answer IR_5__MP_6(answer, alphabet);
  answer IR_5__ML_6(answer, alphabet);
  answer IR_5__MR_6(answer, alphabet);
  answer IR_5__D_6(answer, alphabet);
  answer MP_6__IL_6(alphabet, answer, alphabet);
  answer MP_6__IR_6(alphabet, answer, alphabet);
  answer MP_6__MP_7(alphabet, answer, alphabet);
  answer MP_6__ML_7(alphabet, answer, alphabet);
  answer MP_6__MR_7(alphabet, answer, alphabet);
  answer MP_6__D_7(alphabet, answer, alphabet);
  answer MP_6__EL(alphabet, Subsequence, alphabet);
  answer ML_6__IL_6(alphabet, answer);
  answer ML_6__IR_6(alphabet, answer);
  answer ML_6__MP_7(alphabet, answer);
  answer ML_6__ML_7(alphabet, answer);
  answer ML_6__MR_7(alphabet, answer);
  answer ML_6__D_7(alphabet, answer);
  answer MR_6__IL_6(answer, alphabet);
  answer MR_6__IR_6(answer, alphabet);
  answer MR_6__MP_7(answer, alphabet);
  answer MR_6__ML_7(answer, alphabet);
  answer MR_6__MR_7(answer, alphabet);
  answer MR_6__D_7(answer, alphabet);
  answer D_6__IL_6(answer);
  answer D_6__IR_6(answer);
  answer D_6__MP_7(answer);
  answer D_6__ML_7(answer);
  answer D_6__MR_7(answer);
  answer D_6__D_7(answer);
  answer IL_6__IL_6(alphabet, answer);
  answer IL_6__IR_6(alphabet, answer);
  answer IL_6__MP_7(alphabet, answer);
  answer IL_6__ML_7(alphabet, answer);
  answer IL_6__MR_7(alphabet, answer);
  answer IL_6__D_7(alphabet, answer);
  answer IR_6__IR_6(answer, alphabet);
  answer IR_6__MP_7(answer, alphabet);
  answer IR_6__ML_7(answer, alphabet);
  answer IR_6__MR_7(answer, alphabet);
  answer IR_6__D_7(answer, alphabet);
  answer MP_7__IL_7(alphabet, answer, alphabet);
  answer MP_7__IR_7(alphabet, answer, alphabet);
  answer MP_7__MP_8(alphabet, answer, alphabet);
  answer MP_7__ML_8(alphabet, answer, alphabet);
  answer MP_7__MR_8(alphabet, answer, alphabet);
  answer MP_7__D_8(alphabet, answer, alphabet);
  answer MP_7__EL(alphabet, Subsequence, alphabet);
  answer ML_7__IL_7(alphabet, answer);
  answer ML_7__IR_7(alphabet, answer);
  answer ML_7__MP_8(alphabet, answer);
  answer ML_7__ML_8(alphabet, answer);
  answer ML_7__MR_8(alphabet, answer);
  answer ML_7__D_8(alphabet, answer);
  answer MR_7__IL_7(answer, alphabet);
  answer MR_7__IR_7(answer, alphabet);
  answer MR_7__MP_8(answer, alphabet);
  answer MR_7__ML_8(answer, alphabet);
  answer MR_7__MR_8(answer, alphabet);
  answer MR_7__D_8(answer, alphabet);
  answer D_7__IL_7(answer);
  answer D_7__IR_7(answer);
  answer D_7__MP_8(answer);
  answer D_7__ML_8(answer);
  answer D_7__MR_8(answer);
  answer D_7__D_8(answer);
  answer IL_7__IL_7(alphabet, answer);
  answer IL_7__IR_7(alphabet, answer);
  answer IL_7__MP_8(alphabet, answer);
  answer IL_7__ML_8(alphabet, answer);
  answer IL_7__MR_8(alphabet, answer);
  answer IL_7__D_8(alphabet, answer);
  answer IR_7__IR_7(answer, alphabet);
  answer IR_7__MP_8(answer, alphabet);
  answer IR_7__ML_8(answer, alphabet);
  answer IR_7__MR_8(answer, alphabet);
  answer IR_7__D_8(answer, alphabet);
  answer MP_8__IL_8(alphabet, answer, alphabet);
  answer MP_8__IR_8(alphabet, answer, alphabet);
  answer MP_8__ML_9(alphabet, answer, alphabet);
  answer MP_8__D_9(alphabet, answer, alphabet);
  answer MP_8__EL(alphabet, Subsequence, alphabet);
  answer ML_8__IL_8(alphabet, answer);
  answer ML_8__IR_8(alphabet, answer);
  answer ML_8__ML_9(alphabet, answer);
  answer ML_8__D_9(alphabet, answer);
  answer MR_8__IL_8(answer, alphabet);
  answer MR_8__IR_8(answer, alphabet);
  answer MR_8__ML_9(answer, alphabet);
  answer MR_8__D_9(answer, alphabet);
  answer D_8__IL_8(answer);
  answer D_8__IR_8(answer);
  answer D_8__ML_9(answer);
  answer D_8__D_9(answer);
  answer IL_8__IL_8(alphabet, answer);
  answer IL_8__IR_8(alphabet, answer);
  answer IL_8__ML_9(alphabet, answer);
  answer IL_8__D_9(alphabet, answer);
  answer IR_8__IR_8(answer, alphabet);
  answer IR_8__ML_9(answer, alphabet);
  answer IR_8__D_9(answer, alphabet);
  answer ML_9__IL_9(alphabet, answer);
  answer ML_9__ML_10(alphabet, answer);
  answer ML_9__D_10(alphabet, answer);
  answer ML_9__EL(alphabet, Subsequence);
  answer D_9__IL_9(answer);
  answer D_9__ML_10(answer);
  answer D_9__D_10(answer);
  answer IL_9__IL_9(alphabet, answer);
  answer IL_9__ML_10(alphabet, answer);
  answer IL_9__D_10(alphabet, answer);
  answer ML_10__IL_10(alphabet, answer);
  answer ML_10__ML_11(alphabet, answer);
  answer ML_10__D_11(alphabet, answer);
  answer ML_10__EL(alphabet, Subsequence);
  answer D_10__IL_10(answer);
  answer D_10__ML_11(answer);
  answer D_10__D_11(answer);
  answer IL_10__IL_10(alphabet, answer);
  answer IL_10__ML_11(alphabet, answer);
  answer IL_10__D_11(alphabet, answer);
  answer ML_11__IL_11(alphabet, answer);
  answer ML_11__ML_12(alphabet, answer);
  answer ML_11__D_12(alphabet, answer);
  answer ML_11__EL(alphabet, Subsequence);
  answer D_11__IL_11(answer);
  answer D_11__ML_12(answer);
  answer D_11__D_12(answer);
  answer IL_11__IL_11(alphabet, answer);
  answer IL_11__ML_12(alphabet, answer);
  answer IL_11__D_12(alphabet, answer);
  answer ML_12__E_13(alphabet, answer);
  answer D_12__E_13(answer);
  choice [answer] h([answer]);
}

algebra cyk implements Algebra(alphabet = char, answer = float) {
	float skipl(char a, float x) {
		return x;
	}
	float skipr(float x, char a) {
		return x;
	}
  float window(Subsequence l, float x, Subsequence r) {
    return x;
  }

  float root__global(float x) {
    return x + -0.07400058;
  }

  float root__local(float x) {
    return x + -7.78135971;
  }

  float nil(void) {
    return 0.0;
  }

  float local__MP_2(float x) {
    return x;
  }

  float local__MP_3(float x) {
    return x;
  }

  float local__MP_4(float x) {
    return x;
  }

  float local__MR_5(float x) {
    return x;
  }

  float local__MP_6(float x) {
    return x;
  }

  float local__MP_7(float x) {
    return x;
  }

  float local__MP_8(float x) {
    return x;
  }

  float local__ML_9(float x) {
    return x;
  }

  float local__ML_10(float x) {
    return x;
  }

  float local__ML_11(float x) {
    return x;
  }

  float local__ML_12(float x) {
    return x;
  }

  float MP_1__IL_1(char a, float x, char b) {
    float e = 0.0;
    if ((a == 'A') && (b == 'A')) e = -4.04799663;
    if ((a == 'A') && (b == 'C')) e = 0.15500337;
    if ((a == 'A') && (b == 'G')) e = -4.56099663;
    if ((a == 'A') && (b == 'U')) e = 2.10500337;
    if ((a == 'C') && (b == 'A')) e = -4.77999663;
    if ((a == 'C') && (b == 'C')) e = 0.08000337;
    if ((a == 'C') && (b == 'G')) e = -0.83799663;
    if ((a == 'C') && (b == 'U')) e = -4.26299663;
    if ((a == 'G') && (b == 'A')) e = 0.13200337;
    if ((a == 'G') && (b == 'C')) e = 2.38500337;
    if ((a == 'G') && (b == 'G')) e = -4.04299663;
    if ((a == 'G') && (b == 'U')) e = 0.55100337;
    if ((a == 'U') && (b == 'A')) e = -0.80699663;
    if ((a == 'U') && (b == 'C')) e = -4.01799663;
    if ((a == 'U') && (b == 'G')) e = -2.22299663;
    if ((a == 'U') && (b == 'U')) e = -3.56799663;
    return x + e + -10.73039122;
  }

  float MP_1__IR_1(char a, float x, char b) {
    float e = 0.0;
    if ((a == 'A') && (b == 'A')) e = -4.04799663;
    if ((a == 'A') && (b == 'C')) e = 0.15500337;
    if ((a == 'A') && (b == 'G')) e = -4.56099663;
    if ((a == 'A') && (b == 'U')) e = 2.10500337;
    if ((a == 'C') && (b == 'A')) e = -4.77999663;
    if ((a == 'C') && (b == 'C')) e = 0.08000337;
    if ((a == 'C') && (b == 'G')) e = -0.83799663;
    if ((a == 'C') && (b == 'U')) e = -4.26299663;
    if ((a == 'G') && (b == 'A')) e = 0.13200337;
    if ((a == 'G') && (b == 'C')) e = 2.38500337;
    if ((a == 'G') && (b == 'G')) e = -4.04299663;
    if ((a == 'G') && (b == 'U')) e = 0.55100337;
    if ((a == 'U') && (b == 'A')) e = -0.80699663;
    if ((a == 'U') && (b == 'C')) e = -4.01799663;
    if ((a == 'U') && (b == 'G')) e = -2.22299663;
    if ((a == 'U') && (b == 'U')) e = -3.56799663;
    return x + e + -10.66939122;
  }

  float MP_1__MP_2(char a, float x, char b) {
    float e = 0.0;
    if ((a == 'A') && (b == 'A')) e = -4.04799663;
    if ((a == 'A') && (b == 'C')) e = 0.15500337;
    if ((a == 'A') && (b == 'G')) e = -4.56099663;
    if ((a == 'A') && (b == 'U')) e = 2.10500337;
    if ((a == 'C') && (b == 'A')) e = -4.77999663;
    if ((a == 'C') && (b == 'C')) e = 0.08000337;
    if ((a == 'C') && (b == 'G')) e = -0.83799663;
    if ((a == 'C') && (b == 'U')) e = -4.26299663;
    if ((a == 'G') && (b == 'A')) e = 0.13200337;
    if ((a == 'G') && (b == 'C')) e = 2.38500337;
    if ((a == 'G') && (b == 'G')) e = -4.04299663;
    if ((a == 'G') && (b == 'U')) e = 0.55100337;
    if ((a == 'U') && (b == 'A')) e = -0.80699663;
    if ((a == 'U') && (b == 'C')) e = -4.01799663;
    if ((a == 'U') && (b == 'G')) e = -2.22299663;
    if ((a == 'U') && (b == 'U')) e = -3.56799663;
    return x + e + -0.01339122;
  }

  float MP_1__ML_2(char a, float x, char b) {
    float e = 0.0;
    if ((a == 'A') && (b == 'A')) e = -4.04799663;
    if ((a == 'A') && (b == 'C')) e = 0.15500337;
    if ((a == 'A') && (b == 'G')) e = -4.56099663;
    if ((a == 'A') && (b == 'U')) e = 2.10500337;
    if ((a == 'C') && (b == 'A')) e = -4.77999663;
    if ((a == 'C') && (b == 'C')) e = 0.08000337;
    if ((a == 'C') && (b == 'G')) e = -0.83799663;
    if ((a == 'C') && (b == 'U')) e = -4.26299663;
    if ((a == 'G') && (b == 'A')) e = 0.13200337;
    if ((a == 'G') && (b == 'C')) e = 2.38500337;
    if ((a == 'G') && (b == 'G')) e = -4.04299663;
    if ((a == 'G') && (b == 'U')) e = 0.55100337;
    if ((a == 'U') && (b == 'A')) e = -0.80699663;
    if ((a == 'U') && (b == 'C')) e = -4.01799663;
    if ((a == 'U') && (b == 'G')) e = -2.22299663;
    if ((a == 'U') && (b == 'U')) e = -3.56799663;
    return x + e + -9.44639122;
  }

  float MP_1__MR_2(char a, float x, char b) {
    float e = 0.0;
    if ((a == 'A') && (b == 'A')) e = -4.04799663;
    if ((a == 'A') && (b == 'C')) e = 0.15500337;
    if ((a == 'A') && (b == 'G')) e = -4.56099663;
    if ((a == 'A') && (b == 'U')) e = 2.10500337;
    if ((a == 'C') && (b == 'A')) e = -4.77999663;
    if ((a == 'C') && (b == 'C')) e = 0.08000337;
    if ((a == 'C') && (b == 'G')) e = -0.83799663;
    if ((a == 'C') && (b == 'U')) e = -4.26299663;
    if ((a == 'G') && (b == 'A')) e = 0.13200337;
    if ((a == 'G') && (b == 'C')) e = 2.38500337;
    if ((a == 'G') && (b == 'G')) e = -4.04299663;
    if ((a == 'G') && (b == 'U')) e = 0.55100337;
    if ((a == 'U') && (b == 'A')) e = -0.80699663;
    if ((a == 'U') && (b == 'C')) e = -4.01799663;
    if ((a == 'U') && (b == 'G')) e = -2.22299663;
    if ((a == 'U') && (b == 'U')) e = -3.56799663;
    return x + e + -9.72639122;
  }

  float MP_1__D_2(char a, float x, char b) {
    float e = 0.0;
    if ((a == 'A') && (b == 'A')) e = -4.04799663;
    if ((a == 'A') && (b == 'C')) e = 0.15500337;
    if ((a == 'A') && (b == 'G')) e = -4.56099663;
    if ((a == 'A') && (b == 'U')) e = 2.10500337;
    if ((a == 'C') && (b == 'A')) e = -4.77999663;
    if ((a == 'C') && (b == 'C')) e = 0.08000337;
    if ((a == 'C') && (b == 'G')) e = -0.83799663;
    if ((a == 'C') && (b == 'U')) e = -4.26299663;
    if ((a == 'G') && (b == 'A')) e = 0.13200337;
    if ((a == 'G') && (b == 'C')) e = 2.38500337;
    if ((a == 'G') && (b == 'G')) e = -4.04299663;
    if ((a == 'G') && (b == 'U')) e = 0.55100337;
    if ((a == 'U') && (b == 'A')) e = -0.80699663;
    if ((a == 'U') && (b == 'C')) e = -4.01799663;
    if ((a == 'U') && (b == 'G')) e = -2.22299663;
    if ((a == 'U') && (b == 'U')) e = -3.56799663;
    return x + e + -10.12139122;
  }

  float MP_1__EL(char a, Subsequence x, char b) {
    float e = 0.0;
    if ((a == 'A') && (b == 'A')) e = -4.04799663;
    if ((a == 'A') && (b == 'C')) e = 0.15500337;
    if ((a == 'A') && (b == 'G')) e = -4.56099663;
    if ((a == 'A') && (b == 'U')) e = 2.10500337;
    if ((a == 'C') && (b == 'A')) e = -4.77999663;
    if ((a == 'C') && (b == 'C')) e = 0.08000337;
    if ((a == 'C') && (b == 'G')) e = -0.83799663;
    if ((a == 'C') && (b == 'U')) e = -4.26299663;
    if ((a == 'G') && (b == 'A')) e = 0.13200337;
    if ((a == 'G') && (b == 'C')) e = 2.38500337;
    if ((a == 'G') && (b == 'G')) e = -4.04299663;
    if ((a == 'G') && (b == 'U')) e = 0.55100337;
    if ((a == 'U') && (b == 'A')) e = -0.80699663;
    if ((a == 'U') && (b == 'C')) e = -4.01799663;
    if ((a == 'U') && (b == 'G')) e = -2.22299663;
    if ((a == 'U') && (b == 'U')) e = -3.56799663;
    return e + -7.78135971 + size(x) * -0.08926734;
  }

  float IL_1__IL_1(char a, float x) {
    float e = 0.0;
    if (a == 'A') e = 0.00000000;
    if (a == 'C') e = 0.00000000;
    if (a == 'G') e = 0.00000000;
    if (a == 'U') e = 0.00000000;
    return x + e + -2.57923676;
  }

  float IL_1__IR_1(char a, float x) {
    float e = 0.0;
    if (a == 'A') e = 0.00000000;
    if (a == 'C') e = 0.00000000;
    if (a == 'G') e = 0.00000000;
    if (a == 'U') e = 0.00000000;
    return x + e + -2.84223676;
  }

  float IL_1__MP_2(char a, float x) {
    float e = 0.0;
    if (a == 'A') e = 0.00000000;
    if (a == 'C') e = 0.00000000;
    if (a == 'G') e = 0.00000000;
    if (a == 'U') e = 0.00000000;
    return x + e + -0.76023676;
  }

  float IL_1__ML_2(char a, float x) {
    float e = 0.0;
    if (a == 'A') e = 0.00000000;
    if (a == 'C') e = 0.00000000;
    if (a == 'G') e = 0.00000000;
    if (a == 'U') e = 0.00000000;
    return x + e + -4.49723676;
  }

  float IL_1__MR_2(char a, float x) {
    float e = 0.0;
    if (a == 'A') e = 0.00000000;
    if (a == 'C') e = 0.00000000;
    if (a == 'G') e = 0.00000000;
    if (a == 'U') e = 0.00000000;
    return x + e + -5.27423676;
  }

  float IL_1__D_2(char a, float x) {
    float e = 0.0;
    if (a == 'A') e = 0.00000000;
    if (a == 'C') e = 0.00000000;
    if (a == 'G') e = 0.00000000;
    if (a == 'U') e = 0.00000000;
    return x + e + -4.93423676;
  }

  float IR_1__IR_1(float x, char b) {
    float e = 0.0;
    if (b == 'A') e = 0.00000000;
    if (b == 'C') e = 0.00000000;
    if (b == 'G') e = 0.00000000;
    if (b == 'U') e = 0.00000000;
    return x + e + -2.40826286;
  }

  float IR_1__MP_2(float x, char b) {
    float e = 0.0;
    if (b == 'A') e = 0.00000000;
    if (b == 'C') e = 0.00000000;
    if (b == 'G') e = 0.00000000;
    if (b == 'U') e = 0.00000000;
    return x + e + -0.49626286;
  }

  float IR_1__ML_2(float x, char b) {
    float e = 0.0;
    if (b == 'A') e = 0.00000000;
    if (b == 'C') e = 0.00000000;
    if (b == 'G') e = 0.00000000;
    if (b == 'U') e = 0.00000000;
    return x + e + -5.92026286;
  }

  float IR_1__MR_2(float x, char b) {
    float e = 0.0;
    if (b == 'A') e = 0.00000000;
    if (b == 'C') e = 0.00000000;
    if (b == 'G') e = 0.00000000;
    if (b == 'U') e = 0.00000000;
    return x + e + -4.08726286;
  }

  float IR_1__D_2(float x, char b) {
    float e = 0.0;
    if (b == 'A') e = 0.00000000;
    if (b == 'C') e = 0.00000000;
    if (b == 'G') e = 0.00000000;
    if (b == 'U') e = 0.00000000;
    return x + e + -5.19326286;
  }

  float MP_2__IL_2(char a, float x, char b) {
    float e = 0.0;
    if ((a == 'A') && (b == 'A')) e = -9.47460931;
    if ((a == 'A') && (b == 'C')) e = -4.94860931;
    if ((a == 'A') && (b == 'G')) e = -9.86260931;
    if ((a == 'A') && (b == 'U')) e = -1.76860931;
    if ((a == 'C') && (b == 'A')) e = -9.17560931;
    if ((a == 'C') && (b == 'C')) e = -5.22560931;
    if ((a == 'C') && (b == 'G')) e = -3.12160931;
    if ((a == 'C') && (b == 'U')) e = -8.29460931;
    if ((a == 'G') && (b == 'A')) e = -6.09760931;
    if ((a == 'G') && (b == 'C')) e = 3.92039069;
    if ((a == 'G') && (b == 'G')) e = -5.58060931;
    if ((a == 'G') && (b == 'U')) e = -2.63760931;
    if ((a == 'U') && (b == 'A')) e = -3.01360931;
    if ((a == 'U') && (b == 'C')) e = -5.30660931;
    if ((a == 'U') && (b == 'G')) e = -4.97960931;
    if ((a == 'U') && (b == 'U')) e = -7.13660931;
    return x + e + -10.73039122;
  }

  float MP_2__IR_2(char a, float x, char b) {
    float e = 0.0;
    if ((a == 'A') && (b == 'A')) e = -9.47460931;
    if ((a == 'A') && (b == 'C')) e = -4.94860931;
    if ((a == 'A') && (b == 'G')) e = -9.86260931;
    if ((a == 'A') && (b == 'U')) e = -1.76860931;
    if ((a == 'C') && (b == 'A')) e = -9.17560931;
    if ((a == 'C') && (b == 'C')) e = -5.22560931;
    if ((a == 'C') && (b == 'G')) e = -3.12160931;
    if ((a == 'C') && (b == 'U')) e = -8.29460931;
    if ((a == 'G') && (b == 'A')) e = -6.09760931;
    if ((a == 'G') && (b == 'C')) e = 3.92039069;
    if ((a == 'G') && (b == 'G')) e = -5.58060931;
    if ((a == 'G') && (b == 'U')) e = -2.63760931;
    if ((a == 'U') && (b == 'A')) e = -3.01360931;
    if ((a == 'U') && (b == 'C')) e = -5.30660931;
    if ((a == 'U') && (b == 'G')) e = -4.97960931;
    if ((a == 'U') && (b == 'U')) e = -7.13660931;
    return x + e + -10.66939122;
  }

  float MP_2__MP_3(char a, float x, char b) {
    float e = 0.0;
    if ((a == 'A') && (b == 'A')) e = -9.47460931;
    if ((a == 'A') && (b == 'C')) e = -4.94860931;
    if ((a == 'A') && (b == 'G')) e = -9.86260931;
    if ((a == 'A') && (b == 'U')) e = -1.76860931;
    if ((a == 'C') && (b == 'A')) e = -9.17560931;
    if ((a == 'C') && (b == 'C')) e = -5.22560931;
    if ((a == 'C') && (b == 'G')) e = -3.12160931;
    if ((a == 'C') && (b == 'U')) e = -8.29460931;
    if ((a == 'G') && (b == 'A')) e = -6.09760931;
    if ((a == 'G') && (b == 'C')) e = 3.92039069;
    if ((a == 'G') && (b == 'G')) e = -5.58060931;
    if ((a == 'G') && (b == 'U')) e = -2.63760931;
    if ((a == 'U') && (b == 'A')) e = -3.01360931;
    if ((a == 'U') && (b == 'C')) e = -5.30660931;
    if ((a == 'U') && (b == 'G')) e = -4.97960931;
    if ((a == 'U') && (b == 'U')) e = -7.13660931;
    return x + e + -0.01339122;
  }

  float MP_2__ML_3(char a, float x, char b) {
    float e = 0.0;
    if ((a == 'A') && (b == 'A')) e = -9.47460931;
    if ((a == 'A') && (b == 'C')) e = -4.94860931;
    if ((a == 'A') && (b == 'G')) e = -9.86260931;
    if ((a == 'A') && (b == 'U')) e = -1.76860931;
    if ((a == 'C') && (b == 'A')) e = -9.17560931;
    if ((a == 'C') && (b == 'C')) e = -5.22560931;
    if ((a == 'C') && (b == 'G')) e = -3.12160931;
    if ((a == 'C') && (b == 'U')) e = -8.29460931;
    if ((a == 'G') && (b == 'A')) e = -6.09760931;
    if ((a == 'G') && (b == 'C')) e = 3.92039069;
    if ((a == 'G') && (b == 'G')) e = -5.58060931;
    if ((a == 'G') && (b == 'U')) e = -2.63760931;
    if ((a == 'U') && (b == 'A')) e = -3.01360931;
    if ((a == 'U') && (b == 'C')) e = -5.30660931;
    if ((a == 'U') && (b == 'G')) e = -4.97960931;
    if ((a == 'U') && (b == 'U')) e = -7.13660931;
    return x + e + -9.44639122;
  }

  float MP_2__MR_3(char a, float x, char b) {
    float e = 0.0;
    if ((a == 'A') && (b == 'A')) e = -9.47460931;
    if ((a == 'A') && (b == 'C')) e = -4.94860931;
    if ((a == 'A') && (b == 'G')) e = -9.86260931;
    if ((a == 'A') && (b == 'U')) e = -1.76860931;
    if ((a == 'C') && (b == 'A')) e = -9.17560931;
    if ((a == 'C') && (b == 'C')) e = -5.22560931;
    if ((a == 'C') && (b == 'G')) e = -3.12160931;
    if ((a == 'C') && (b == 'U')) e = -8.29460931;
    if ((a == 'G') && (b == 'A')) e = -6.09760931;
    if ((a == 'G') && (b == 'C')) e = 3.92039069;
    if ((a == 'G') && (b == 'G')) e = -5.58060931;
    if ((a == 'G') && (b == 'U')) e = -2.63760931;
    if ((a == 'U') && (b == 'A')) e = -3.01360931;
    if ((a == 'U') && (b == 'C')) e = -5.30660931;
    if ((a == 'U') && (b == 'G')) e = -4.97960931;
    if ((a == 'U') && (b == 'U')) e = -7.13660931;
    return x + e + -9.72639122;
  }

  float MP_2__D_3(char a, float x, char b) {
    float e = 0.0;
    if ((a == 'A') && (b == 'A')) e = -9.47460931;
    if ((a == 'A') && (b == 'C')) e = -4.94860931;
    if ((a == 'A') && (b == 'G')) e = -9.86260931;
    if ((a == 'A') && (b == 'U')) e = -1.76860931;
    if ((a == 'C') && (b == 'A')) e = -9.17560931;
    if ((a == 'C') && (b == 'C')) e = -5.22560931;
    if ((a == 'C') && (b == 'G')) e = -3.12160931;
    if ((a == 'C') && (b == 'U')) e = -8.29460931;
    if ((a == 'G') && (b == 'A')) e = -6.09760931;
    if ((a == 'G') && (b == 'C')) e = 3.92039069;
    if ((a == 'G') && (b == 'G')) e = -5.58060931;
    if ((a == 'G') && (b == 'U')) e = -2.63760931;
    if ((a == 'U') && (b == 'A')) e = -3.01360931;
    if ((a == 'U') && (b == 'C')) e = -5.30660931;
    if ((a == 'U') && (b == 'G')) e = -4.97960931;
    if ((a == 'U') && (b == 'U')) e = -7.13660931;
    return x + e + -10.12139122;
  }

  float MP_2__EL(char a, Subsequence x, char b) {
    float e = 0.0;
    if ((a == 'A') && (b == 'A')) e = -9.47460931;
    if ((a == 'A') && (b == 'C')) e = -4.94860931;
    if ((a == 'A') && (b == 'G')) e = -9.86260931;
    if ((a == 'A') && (b == 'U')) e = -1.76860931;
    if ((a == 'C') && (b == 'A')) e = -9.17560931;
    if ((a == 'C') && (b == 'C')) e = -5.22560931;
    if ((a == 'C') && (b == 'G')) e = -3.12160931;
    if ((a == 'C') && (b == 'U')) e = -8.29460931;
    if ((a == 'G') && (b == 'A')) e = -6.09760931;
    if ((a == 'G') && (b == 'C')) e = 3.92039069;
    if ((a == 'G') && (b == 'G')) e = -5.58060931;
    if ((a == 'G') && (b == 'U')) e = -2.63760931;
    if ((a == 'U') && (b == 'A')) e = -3.01360931;
    if ((a == 'U') && (b == 'C')) e = -5.30660931;
    if ((a == 'U') && (b == 'G')) e = -4.97960931;
    if ((a == 'U') && (b == 'U')) e = -7.13660931;
    return e + -7.78135971 + size(x) * -0.08926734;
  }

  float ML_2__IL_2(char a, float x) {
    float e = 0.0;
    if (a == 'A') e = 0.66026728;
    if (a == 'C') e = -0.61173272;
    if (a == 'G') e = -0.29273272;
    if (a == 'U') e = -0.07573272;
    return x + e + -6.25018711;
  }

  float ML_2__IR_2(char a, float x) {
    float e = 0.0;
    if (a == 'A') e = 0.66026728;
    if (a == 'C') e = -0.61173272;
    if (a == 'G') e = -0.29273272;
    if (a == 'U') e = -0.07573272;
    return x + e + -6.59618711;
  }

  float ML_2__MP_3(char a, float x) {
    float e = 0.0;
    if (a == 'A') e = 0.66026728;
    if (a == 'C') e = -0.61173272;
    if (a == 'G') e = -0.29273272;
    if (a == 'U') e = -0.07573272;
    return x + e + -1.31018711;
  }

  float ML_2__ML_3(char a, float x) {
    float e = 0.0;
    if (a == 'A') e = 0.66026728;
    if (a == 'C') e = -0.61173272;
    if (a == 'G') e = -0.29273272;
    if (a == 'U') e = -0.07573272;
    return x + e + -1.00518711;
  }

  float ML_2__MR_3(char a, float x) {
    float e = 0.0;
    if (a == 'A') e = 0.66026728;
    if (a == 'C') e = -0.61173272;
    if (a == 'G') e = -0.29273272;
    if (a == 'U') e = -0.07573272;
    return x + e + -6.44618711;
  }

  float ML_2__D_3(char a, float x) {
    float e = 0.0;
    if (a == 'A') e = 0.66026728;
    if (a == 'C') e = -0.61173272;
    if (a == 'G') e = -0.29273272;
    if (a == 'U') e = -0.07573272;
    return x + e + -3.97518711;
  }

  float MR_2__IL_2(float x, char b) {
    float e = 0.0;
    if (b == 'A') e = 0.66026728;
    if (b == 'C') e = -0.61173272;
    if (b == 'G') e = -0.29273272;
    if (b == 'U') e = -0.07573272;
    return x + e + -6.98790919;
  }

  float MR_2__IR_2(float x, char b) {
    float e = 0.0;
    if (b == 'A') e = 0.66026728;
    if (b == 'C') e = -0.61173272;
    if (b == 'G') e = -0.29273272;
    if (b == 'U') e = -0.07573272;
    return x + e + -5.71690919;
  }

  float MR_2__MP_3(float x, char b) {
    float e = 0.0;
    if (b == 'A') e = 0.66026728;
    if (b == 'C') e = -0.61173272;
    if (b == 'G') e = -0.29273272;
    if (b == 'U') e = -0.07573272;
    return x + e + -1.62490919;
  }

  float MR_2__ML_3(float x, char b) {
    float e = 0.0;
    if (b == 'A') e = 0.66026728;
    if (b == 'C') e = -0.61173272;
    if (b == 'G') e = -0.29273272;
    if (b == 'U') e = -0.07573272;
    return x + e + -5.69490919;
  }

  float MR_2__MR_3(float x, char b) {
    float e = 0.0;
    if (b == 'A') e = 0.66026728;
    if (b == 'C') e = -0.61173272;
    if (b == 'G') e = -0.29273272;
    if (b == 'U') e = -0.07573272;
    return x + e + -0.82890919;
  }

  float MR_2__D_3(float x, char b) {
    float e = 0.0;
    if (b == 'A') e = 0.66026728;
    if (b == 'C') e = -0.61173272;
    if (b == 'G') e = -0.29273272;
    if (b == 'U') e = -0.07573272;
    return x + e + -3.90790919;
  }

  float D_2__IL_2(float x) {
    return x + -9.04916484;
  }

  float D_2__IR_2(float x) {
    return x + -7.74716484;
  }

  float D_2__MP_3(float x) {
    return x + -3.54416484;
  }

  float D_2__ML_3(float x) {
    return x + -4.22616484;
  }

  float D_2__MR_3(float x) {
    return x + -4.24416484;
  }

  float D_2__D_3(float x) {
    return x + -0.31916484;
  }

  float IL_2__IL_2(char a, float x) {
    float e = 0.0;
    if (a == 'A') e = 0.00000000;
    if (a == 'C') e = 0.00000000;
    if (a == 'G') e = 0.00000000;
    if (a == 'U') e = 0.00000000;
    return x + e + -2.57923676;
  }

  float IL_2__IR_2(char a, float x) {
    float e = 0.0;
    if (a == 'A') e = 0.00000000;
    if (a == 'C') e = 0.00000000;
    if (a == 'G') e = 0.00000000;
    if (a == 'U') e = 0.00000000;
    return x + e + -2.84223676;
  }

  float IL_2__MP_3(char a, float x) {
    float e = 0.0;
    if (a == 'A') e = 0.00000000;
    if (a == 'C') e = 0.00000000;
    if (a == 'G') e = 0.00000000;
    if (a == 'U') e = 0.00000000;
    return x + e + -0.76023676;
  }

  float IL_2__ML_3(char a, float x) {
    float e = 0.0;
    if (a == 'A') e = 0.00000000;
    if (a == 'C') e = 0.00000000;
    if (a == 'G') e = 0.00000000;
    if (a == 'U') e = 0.00000000;
    return x + e + -4.49723676;
  }

  float IL_2__MR_3(char a, float x) {
    float e = 0.0;
    if (a == 'A') e = 0.00000000;
    if (a == 'C') e = 0.00000000;
    if (a == 'G') e = 0.00000000;
    if (a == 'U') e = 0.00000000;
    return x + e + -5.27423676;
  }

  float IL_2__D_3(char a, float x) {
    float e = 0.0;
    if (a == 'A') e = 0.00000000;
    if (a == 'C') e = 0.00000000;
    if (a == 'G') e = 0.00000000;
    if (a == 'U') e = 0.00000000;
    return x + e + -4.93423676;
  }

  float IR_2__IR_2(float x, char b) {
    float e = 0.0;
    if (b == 'A') e = 0.00000000;
    if (b == 'C') e = 0.00000000;
    if (b == 'G') e = 0.00000000;
    if (b == 'U') e = 0.00000000;
    return x + e + -2.40826286;
  }

  float IR_2__MP_3(float x, char b) {
    float e = 0.0;
    if (b == 'A') e = 0.00000000;
    if (b == 'C') e = 0.00000000;
    if (b == 'G') e = 0.00000000;
    if (b == 'U') e = 0.00000000;
    return x + e + -0.49626286;
  }

  float IR_2__ML_3(float x, char b) {
    float e = 0.0;
    if (b == 'A') e = 0.00000000;
    if (b == 'C') e = 0.00000000;
    if (b == 'G') e = 0.00000000;
    if (b == 'U') e = 0.00000000;
    return x + e + -5.92026286;
  }

  float IR_2__MR_3(float x, char b) {
    float e = 0.0;
    if (b == 'A') e = 0.00000000;
    if (b == 'C') e = 0.00000000;
    if (b == 'G') e = 0.00000000;
    if (b == 'U') e = 0.00000000;
    return x + e + -4.08726286;
  }

  float IR_2__D_3(float x, char b) {
    float e = 0.0;
    if (b == 'A') e = 0.00000000;
    if (b == 'C') e = 0.00000000;
    if (b == 'G') e = 0.00000000;
    if (b == 'U') e = 0.00000000;
    return x + e + -5.19326286;
  }

  float MP_3__IL_3(char a, float x, char b) {
    float e = 0.0;
    if ((a == 'A') && (b == 'A')) e = -7.42915800;
    if ((a == 'A') && (b == 'C')) e = -7.16115800;
    if ((a == 'A') && (b == 'G')) e = -5.45115800;
    if ((a == 'A') && (b == 'U')) e = -2.86115800;
    if ((a == 'C') && (b == 'A')) e = -5.20415800;
    if ((a == 'C') && (b == 'C')) e = -6.08415800;
    if ((a == 'C') && (b == 'G')) e = 3.90684200;
    if ((a == 'C') && (b == 'U')) e = -6.06915800;
    if ((a == 'G') && (b == 'A')) e = -8.73815800;
    if ((a == 'G') && (b == 'C')) e = -2.86115800;
    if ((a == 'G') && (b == 'G')) e = -5.41815800;
    if ((a == 'G') && (b == 'U')) e = -4.34515800;
    if ((a == 'U') && (b == 'A')) e = -1.43915800;
    if ((a == 'U') && (b == 'C')) e = -7.98415800;
    if ((a == 'U') && (b == 'G')) e = -2.54415800;
    if ((a == 'U') && (b == 'U')) e = -6.18615800;
    return x + e + -10.73039122;
  }

  float MP_3__IR_3(char a, float x, char b) {
    float e = 0.0;
    if ((a == 'A') && (b == 'A')) e = -7.42915800;
    if ((a == 'A') && (b == 'C')) e = -7.16115800;
    if ((a == 'A') && (b == 'G')) e = -5.45115800;
    if ((a == 'A') && (b == 'U')) e = -2.86115800;
    if ((a == 'C') && (b == 'A')) e = -5.20415800;
    if ((a == 'C') && (b == 'C')) e = -6.08415800;
    if ((a == 'C') && (b == 'G')) e = 3.90684200;
    if ((a == 'C') && (b == 'U')) e = -6.06915800;
    if ((a == 'G') && (b == 'A')) e = -8.73815800;
    if ((a == 'G') && (b == 'C')) e = -2.86115800;
    if ((a == 'G') && (b == 'G')) e = -5.41815800;
    if ((a == 'G') && (b == 'U')) e = -4.34515800;
    if ((a == 'U') && (b == 'A')) e = -1.43915800;
    if ((a == 'U') && (b == 'C')) e = -7.98415800;
    if ((a == 'U') && (b == 'G')) e = -2.54415800;
    if ((a == 'U') && (b == 'U')) e = -6.18615800;
    return x + e + -10.66939122;
  }

  float MP_3__MP_4(char a, float x, char b) {
    float e = 0.0;
    if ((a == 'A') && (b == 'A')) e = -7.42915800;
    if ((a == 'A') && (b == 'C')) e = -7.16115800;
    if ((a == 'A') && (b == 'G')) e = -5.45115800;
    if ((a == 'A') && (b == 'U')) e = -2.86115800;
    if ((a == 'C') && (b == 'A')) e = -5.20415800;
    if ((a == 'C') && (b == 'C')) e = -6.08415800;
    if ((a == 'C') && (b == 'G')) e = 3.90684200;
    if ((a == 'C') && (b == 'U')) e = -6.06915800;
    if ((a == 'G') && (b == 'A')) e = -8.73815800;
    if ((a == 'G') && (b == 'C')) e = -2.86115800;
    if ((a == 'G') && (b == 'G')) e = -5.41815800;
    if ((a == 'G') && (b == 'U')) e = -4.34515800;
    if ((a == 'U') && (b == 'A')) e = -1.43915800;
    if ((a == 'U') && (b == 'C')) e = -7.98415800;
    if ((a == 'U') && (b == 'G')) e = -2.54415800;
    if ((a == 'U') && (b == 'U')) e = -6.18615800;
    return x + e + -0.01339122;
  }

  float MP_3__ML_4(char a, float x, char b) {
    float e = 0.0;
    if ((a == 'A') && (b == 'A')) e = -7.42915800;
    if ((a == 'A') && (b == 'C')) e = -7.16115800;
    if ((a == 'A') && (b == 'G')) e = -5.45115800;
    if ((a == 'A') && (b == 'U')) e = -2.86115800;
    if ((a == 'C') && (b == 'A')) e = -5.20415800;
    if ((a == 'C') && (b == 'C')) e = -6.08415800;
    if ((a == 'C') && (b == 'G')) e = 3.90684200;
    if ((a == 'C') && (b == 'U')) e = -6.06915800;
    if ((a == 'G') && (b == 'A')) e = -8.73815800;
    if ((a == 'G') && (b == 'C')) e = -2.86115800;
    if ((a == 'G') && (b == 'G')) e = -5.41815800;
    if ((a == 'G') && (b == 'U')) e = -4.34515800;
    if ((a == 'U') && (b == 'A')) e = -1.43915800;
    if ((a == 'U') && (b == 'C')) e = -7.98415800;
    if ((a == 'U') && (b == 'G')) e = -2.54415800;
    if ((a == 'U') && (b == 'U')) e = -6.18615800;
    return x + e + -9.44639122;
  }

  float MP_3__MR_4(char a, float x, char b) {
    float e = 0.0;
    if ((a == 'A') && (b == 'A')) e = -7.42915800;
    if ((a == 'A') && (b == 'C')) e = -7.16115800;
    if ((a == 'A') && (b == 'G')) e = -5.45115800;
    if ((a == 'A') && (b == 'U')) e = -2.86115800;
    if ((a == 'C') && (b == 'A')) e = -5.20415800;
    if ((a == 'C') && (b == 'C')) e = -6.08415800;
    if ((a == 'C') && (b == 'G')) e = 3.90684200;
    if ((a == 'C') && (b == 'U')) e = -6.06915800;
    if ((a == 'G') && (b == 'A')) e = -8.73815800;
    if ((a == 'G') && (b == 'C')) e = -2.86115800;
    if ((a == 'G') && (b == 'G')) e = -5.41815800;
    if ((a == 'G') && (b == 'U')) e = -4.34515800;
    if ((a == 'U') && (b == 'A')) e = -1.43915800;
    if ((a == 'U') && (b == 'C')) e = -7.98415800;
    if ((a == 'U') && (b == 'G')) e = -2.54415800;
    if ((a == 'U') && (b == 'U')) e = -6.18615800;
    return x + e + -9.72639122;
  }

  float MP_3__D_4(char a, float x, char b) {
    float e = 0.0;
    if ((a == 'A') && (b == 'A')) e = -7.42915800;
    if ((a == 'A') && (b == 'C')) e = -7.16115800;
    if ((a == 'A') && (b == 'G')) e = -5.45115800;
    if ((a == 'A') && (b == 'U')) e = -2.86115800;
    if ((a == 'C') && (b == 'A')) e = -5.20415800;
    if ((a == 'C') && (b == 'C')) e = -6.08415800;
    if ((a == 'C') && (b == 'G')) e = 3.90684200;
    if ((a == 'C') && (b == 'U')) e = -6.06915800;
    if ((a == 'G') && (b == 'A')) e = -8.73815800;
    if ((a == 'G') && (b == 'C')) e = -2.86115800;
    if ((a == 'G') && (b == 'G')) e = -5.41815800;
    if ((a == 'G') && (b == 'U')) e = -4.34515800;
    if ((a == 'U') && (b == 'A')) e = -1.43915800;
    if ((a == 'U') && (b == 'C')) e = -7.98415800;
    if ((a == 'U') && (b == 'G')) e = -2.54415800;
    if ((a == 'U') && (b == 'U')) e = -6.18615800;
    return x + e + -10.12139122;
  }

  float MP_3__EL(char a, Subsequence x, char b) {
    float e = 0.0;
    if ((a == 'A') && (b == 'A')) e = -7.42915800;
    if ((a == 'A') && (b == 'C')) e = -7.16115800;
    if ((a == 'A') && (b == 'G')) e = -5.45115800;
    if ((a == 'A') && (b == 'U')) e = -2.86115800;
    if ((a == 'C') && (b == 'A')) e = -5.20415800;
    if ((a == 'C') && (b == 'C')) e = -6.08415800;
    if ((a == 'C') && (b == 'G')) e = 3.90684200;
    if ((a == 'C') && (b == 'U')) e = -6.06915800;
    if ((a == 'G') && (b == 'A')) e = -8.73815800;
    if ((a == 'G') && (b == 'C')) e = -2.86115800;
    if ((a == 'G') && (b == 'G')) e = -5.41815800;
    if ((a == 'G') && (b == 'U')) e = -4.34515800;
    if ((a == 'U') && (b == 'A')) e = -1.43915800;
    if ((a == 'U') && (b == 'C')) e = -7.98415800;
    if ((a == 'U') && (b == 'G')) e = -2.54415800;
    if ((a == 'U') && (b == 'U')) e = -6.18615800;
    return e + -7.78135971 + size(x) * -0.08926734;
  }

  float ML_3__IL_3(char a, float x) {
    float e = 0.0;
    if (a == 'A') e = 0.66026728;
    if (a == 'C') e = -0.61173272;
    if (a == 'G') e = -0.29273272;
    if (a == 'U') e = -0.07573272;
    return x + e + -6.25018711;
  }

  float ML_3__IR_3(char a, float x) {
    float e = 0.0;
    if (a == 'A') e = 0.66026728;
    if (a == 'C') e = -0.61173272;
    if (a == 'G') e = -0.29273272;
    if (a == 'U') e = -0.07573272;
    return x + e + -6.59618711;
  }

  float ML_3__MP_4(char a, float x) {
    float e = 0.0;
    if (a == 'A') e = 0.66026728;
    if (a == 'C') e = -0.61173272;
    if (a == 'G') e = -0.29273272;
    if (a == 'U') e = -0.07573272;
    return x + e + -1.31018711;
  }

  float ML_3__ML_4(char a, float x) {
    float e = 0.0;
    if (a == 'A') e = 0.66026728;
    if (a == 'C') e = -0.61173272;
    if (a == 'G') e = -0.29273272;
    if (a == 'U') e = -0.07573272;
    return x + e + -1.00518711;
  }

  float ML_3__MR_4(char a, float x) {
    float e = 0.0;
    if (a == 'A') e = 0.66026728;
    if (a == 'C') e = -0.61173272;
    if (a == 'G') e = -0.29273272;
    if (a == 'U') e = -0.07573272;
    return x + e + -6.44618711;
  }

  float ML_3__D_4(char a, float x) {
    float e = 0.0;
    if (a == 'A') e = 0.66026728;
    if (a == 'C') e = -0.61173272;
    if (a == 'G') e = -0.29273272;
    if (a == 'U') e = -0.07573272;
    return x + e + -3.97518711;
  }

  float MR_3__IL_3(float x, char b) {
    float e = 0.0;
    if (b == 'A') e = 0.66026728;
    if (b == 'C') e = -0.61173272;
    if (b == 'G') e = -0.29273272;
    if (b == 'U') e = -0.07573272;
    return x + e + -6.98790919;
  }

  float MR_3__IR_3(float x, char b) {
    float e = 0.0;
    if (b == 'A') e = 0.66026728;
    if (b == 'C') e = -0.61173272;
    if (b == 'G') e = -0.29273272;
    if (b == 'U') e = -0.07573272;
    return x + e + -5.71690919;
  }

  float MR_3__MP_4(float x, char b) {
    float e = 0.0;
    if (b == 'A') e = 0.66026728;
    if (b == 'C') e = -0.61173272;
    if (b == 'G') e = -0.29273272;
    if (b == 'U') e = -0.07573272;
    return x + e + -1.62490919;
  }

  float MR_3__ML_4(float x, char b) {
    float e = 0.0;
    if (b == 'A') e = 0.66026728;
    if (b == 'C') e = -0.61173272;
    if (b == 'G') e = -0.29273272;
    if (b == 'U') e = -0.07573272;
    return x + e + -5.69490919;
  }

  float MR_3__MR_4(float x, char b) {
    float e = 0.0;
    if (b == 'A') e = 0.66026728;
    if (b == 'C') e = -0.61173272;
    if (b == 'G') e = -0.29273272;
    if (b == 'U') e = -0.07573272;
    return x + e + -0.82890919;
  }

  float MR_3__D_4(float x, char b) {
    float e = 0.0;
    if (b == 'A') e = 0.66026728;
    if (b == 'C') e = -0.61173272;
    if (b == 'G') e = -0.29273272;
    if (b == 'U') e = -0.07573272;
    return x + e + -3.90790919;
  }

  float D_3__IL_3(float x) {
    return x + -9.04916484;
  }

  float D_3__IR_3(float x) {
    return x + -7.74716484;
  }

  float D_3__MP_4(float x) {
    return x + -3.54416484;
  }

  float D_3__ML_4(float x) {
    return x + -4.22616484;
  }

  float D_3__MR_4(float x) {
    return x + -4.24416484;
  }

  float D_3__D_4(float x) {
    return x + -0.31916484;
  }

  float IL_3__IL_3(char a, float x) {
    float e = 0.0;
    if (a == 'A') e = 0.00000000;
    if (a == 'C') e = 0.00000000;
    if (a == 'G') e = 0.00000000;
    if (a == 'U') e = 0.00000000;
    return x + e + -2.57923676;
  }

  float IL_3__IR_3(char a, float x) {
    float e = 0.0;
    if (a == 'A') e = 0.00000000;
    if (a == 'C') e = 0.00000000;
    if (a == 'G') e = 0.00000000;
    if (a == 'U') e = 0.00000000;
    return x + e + -2.84223676;
  }

  float IL_3__MP_4(char a, float x) {
    float e = 0.0;
    if (a == 'A') e = 0.00000000;
    if (a == 'C') e = 0.00000000;
    if (a == 'G') e = 0.00000000;
    if (a == 'U') e = 0.00000000;
    return x + e + -0.76023676;
  }

  float IL_3__ML_4(char a, float x) {
    float e = 0.0;
    if (a == 'A') e = 0.00000000;
    if (a == 'C') e = 0.00000000;
    if (a == 'G') e = 0.00000000;
    if (a == 'U') e = 0.00000000;
    return x + e + -4.49723676;
  }

  float IL_3__MR_4(char a, float x) {
    float e = 0.0;
    if (a == 'A') e = 0.00000000;
    if (a == 'C') e = 0.00000000;
    if (a == 'G') e = 0.00000000;
    if (a == 'U') e = 0.00000000;
    return x + e + -5.27423676;
  }

  float IL_3__D_4(char a, float x) {
    float e = 0.0;
    if (a == 'A') e = 0.00000000;
    if (a == 'C') e = 0.00000000;
    if (a == 'G') e = 0.00000000;
    if (a == 'U') e = 0.00000000;
    return x + e + -4.93423676;
  }

  float IR_3__IR_3(float x, char b) {
    float e = 0.0;
    if (b == 'A') e = 0.00000000;
    if (b == 'C') e = 0.00000000;
    if (b == 'G') e = 0.00000000;
    if (b == 'U') e = 0.00000000;
    return x + e + -2.40826286;
  }

  float IR_3__MP_4(float x, char b) {
    float e = 0.0;
    if (b == 'A') e = 0.00000000;
    if (b == 'C') e = 0.00000000;
    if (b == 'G') e = 0.00000000;
    if (b == 'U') e = 0.00000000;
    return x + e + -0.49626286;
  }

  float IR_3__ML_4(float x, char b) {
    float e = 0.0;
    if (b == 'A') e = 0.00000000;
    if (b == 'C') e = 0.00000000;
    if (b == 'G') e = 0.00000000;
    if (b == 'U') e = 0.00000000;
    return x + e + -5.92026286;
  }

  float IR_3__MR_4(float x, char b) {
    float e = 0.0;
    if (b == 'A') e = 0.00000000;
    if (b == 'C') e = 0.00000000;
    if (b == 'G') e = 0.00000000;
    if (b == 'U') e = 0.00000000;
    return x + e + -4.08726286;
  }

  float IR_3__D_4(float x, char b) {
    float e = 0.0;
    if (b == 'A') e = 0.00000000;
    if (b == 'C') e = 0.00000000;
    if (b == 'G') e = 0.00000000;
    if (b == 'U') e = 0.00000000;
    return x + e + -5.19326286;
  }

  float MP_4__IL_4(char a, float x, char b) {
    float e = 0.0;
    if ((a == 'A') && (b == 'A')) e = -9.47460931;
    if ((a == 'A') && (b == 'C')) e = -4.94860931;
    if ((a == 'A') && (b == 'G')) e = -9.86260931;
    if ((a == 'A') && (b == 'U')) e = -1.76860931;
    if ((a == 'C') && (b == 'A')) e = -9.17560931;
    if ((a == 'C') && (b == 'C')) e = -5.22560931;
    if ((a == 'C') && (b == 'G')) e = -3.12160931;
    if ((a == 'C') && (b == 'U')) e = -8.29460931;
    if ((a == 'G') && (b == 'A')) e = -6.09760931;
    if ((a == 'G') && (b == 'C')) e = 3.92039069;
    if ((a == 'G') && (b == 'G')) e = -5.58060931;
    if ((a == 'G') && (b == 'U')) e = -2.63760931;
    if ((a == 'U') && (b == 'A')) e = -3.01360931;
    if ((a == 'U') && (b == 'C')) e = -5.30660931;
    if ((a == 'U') && (b == 'G')) e = -4.97960931;
    if ((a == 'U') && (b == 'U')) e = -7.13660931;
    return x + e + -9.44976242;
  }

  float MP_4__IR_4(char a, float x, char b) {
    float e = 0.0;
    if ((a == 'A') && (b == 'A')) e = -9.47460931;
    if ((a == 'A') && (b == 'C')) e = -4.94860931;
    if ((a == 'A') && (b == 'G')) e = -9.86260931;
    if ((a == 'A') && (b == 'U')) e = -1.76860931;
    if ((a == 'C') && (b == 'A')) e = -9.17560931;
    if ((a == 'C') && (b == 'C')) e = -5.22560931;
    if ((a == 'C') && (b == 'G')) e = -3.12160931;
    if ((a == 'C') && (b == 'U')) e = -8.29460931;
    if ((a == 'G') && (b == 'A')) e = -6.09760931;
    if ((a == 'G') && (b == 'C')) e = 3.92039069;
    if ((a == 'G') && (b == 'G')) e = -5.58060931;
    if ((a == 'G') && (b == 'U')) e = -2.63760931;
    if ((a == 'U') && (b == 'A')) e = -3.01360931;
    if ((a == 'U') && (b == 'C')) e = -5.30660931;
    if ((a == 'U') && (b == 'G')) e = -4.97960931;
    if ((a == 'U') && (b == 'U')) e = -7.13660931;
    return x + e + -10.69576242;
  }

  float MP_4__MR_5(char a, float x, char b) {
    float e = 0.0;
    if ((a == 'A') && (b == 'A')) e = -9.47460931;
    if ((a == 'A') && (b == 'C')) e = -4.94860931;
    if ((a == 'A') && (b == 'G')) e = -9.86260931;
    if ((a == 'A') && (b == 'U')) e = -1.76860931;
    if ((a == 'C') && (b == 'A')) e = -9.17560931;
    if ((a == 'C') && (b == 'C')) e = -5.22560931;
    if ((a == 'C') && (b == 'G')) e = -3.12160931;
    if ((a == 'C') && (b == 'U')) e = -8.29460931;
    if ((a == 'G') && (b == 'A')) e = -6.09760931;
    if ((a == 'G') && (b == 'C')) e = 3.92039069;
    if ((a == 'G') && (b == 'G')) e = -5.58060931;
    if ((a == 'G') && (b == 'U')) e = -2.63760931;
    if ((a == 'U') && (b == 'A')) e = -3.01360931;
    if ((a == 'U') && (b == 'C')) e = -5.30660931;
    if ((a == 'U') && (b == 'G')) e = -4.97960931;
    if ((a == 'U') && (b == 'U')) e = -7.13660931;
    return x + e + -0.01976242;
  }

  float MP_4__D_5(char a, float x, char b) {
    float e = 0.0;
    if ((a == 'A') && (b == 'A')) e = -9.47460931;
    if ((a == 'A') && (b == 'C')) e = -4.94860931;
    if ((a == 'A') && (b == 'G')) e = -9.86260931;
    if ((a == 'A') && (b == 'U')) e = -1.76860931;
    if ((a == 'C') && (b == 'A')) e = -9.17560931;
    if ((a == 'C') && (b == 'C')) e = -5.22560931;
    if ((a == 'C') && (b == 'G')) e = -3.12160931;
    if ((a == 'C') && (b == 'U')) e = -8.29460931;
    if ((a == 'G') && (b == 'A')) e = -6.09760931;
    if ((a == 'G') && (b == 'C')) e = 3.92039069;
    if ((a == 'G') && (b == 'G')) e = -5.58060931;
    if ((a == 'G') && (b == 'U')) e = -2.63760931;
    if ((a == 'U') && (b == 'A')) e = -3.01360931;
    if ((a == 'U') && (b == 'C')) e = -5.30660931;
    if ((a == 'U') && (b == 'G')) e = -4.97960931;
    if ((a == 'U') && (b == 'U')) e = -7.13660931;
    return x + e + -7.14876242;
  }

  float MP_4__EL(char a, Subsequence x, char b) {
    float e = 0.0;
    if ((a == 'A') && (b == 'A')) e = -9.47460931;
    if ((a == 'A') && (b == 'C')) e = -4.94860931;
    if ((a == 'A') && (b == 'G')) e = -9.86260931;
    if ((a == 'A') && (b == 'U')) e = -1.76860931;
    if ((a == 'C') && (b == 'A')) e = -9.17560931;
    if ((a == 'C') && (b == 'C')) e = -5.22560931;
    if ((a == 'C') && (b == 'G')) e = -3.12160931;
    if ((a == 'C') && (b == 'U')) e = -8.29460931;
    if ((a == 'G') && (b == 'A')) e = -6.09760931;
    if ((a == 'G') && (b == 'C')) e = 3.92039069;
    if ((a == 'G') && (b == 'G')) e = -5.58060931;
    if ((a == 'G') && (b == 'U')) e = -2.63760931;
    if ((a == 'U') && (b == 'A')) e = -3.01360931;
    if ((a == 'U') && (b == 'C')) e = -5.30660931;
    if ((a == 'U') && (b == 'G')) e = -4.97960931;
    if ((a == 'U') && (b == 'U')) e = -7.13660931;
    return e + -7.78135971 + size(x) * -0.08926734;
  }

  float ML_4__IL_4(char a, float x) {
    float e = 0.0;
    if (a == 'A') e = 0.66026728;
    if (a == 'C') e = -0.61173272;
    if (a == 'G') e = -0.29273272;
    if (a == 'U') e = -0.07573272;
    return x + e + -2.40796470;
  }

  float ML_4__IR_4(char a, float x) {
    float e = 0.0;
    if (a == 'A') e = 0.66026728;
    if (a == 'C') e = -0.61173272;
    if (a == 'G') e = -0.29273272;
    if (a == 'U') e = -0.07573272;
    return x + e + -4.53196470;
  }

  float ML_4__MR_5(char a, float x) {
    float e = 0.0;
    if (a == 'A') e = 0.66026728;
    if (a == 'C') e = -0.61173272;
    if (a == 'G') e = -0.29273272;
    if (a == 'U') e = -0.07573272;
    return x + e + -1.29296470;
  }

  float ML_4__D_5(char a, float x) {
    float e = 0.0;
    if (a == 'A') e = 0.66026728;
    if (a == 'C') e = -0.61173272;
    if (a == 'G') e = -0.29273272;
    if (a == 'U') e = -0.07573272;
    return x + e + -1.47296470;
  }

  float MR_4__IL_4(float x, char b) {
    float e = 0.0;
    if (b == 'A') e = 0.66026728;
    if (b == 'C') e = -0.61173272;
    if (b == 'G') e = -0.29273272;
    if (b == 'U') e = -0.07573272;
    return x + e + -4.10222643;
  }

  float MR_4__IR_4(float x, char b) {
    float e = 0.0;
    if (b == 'A') e = 0.66026728;
    if (b == 'C') e = -0.61173272;
    if (b == 'G') e = -0.29273272;
    if (b == 'U') e = -0.07573272;
    return x + e + -12.52822643;
  }

  float MR_4__MR_5(float x, char b) {
    float e = 0.0;
    if (b == 'A') e = 0.66026728;
    if (b == 'C') e = -0.61173272;
    if (b == 'G') e = -0.29273272;
    if (b == 'U') e = -0.07573272;
    return x + e + -0.39022643;
  }

  float MR_4__D_5(float x, char b) {
    float e = 0.0;
    if (b == 'A') e = 0.66026728;
    if (b == 'C') e = -0.61173272;
    if (b == 'G') e = -0.29273272;
    if (b == 'U') e = -0.07573272;
    return x + e + -2.48522643;
  }

  float D_4__IL_4(float x) {
    return x + -12.73672016;
  }

  float D_4__IR_4(float x) {
    return x + -14.00672016;
  }

  float D_4__MR_5(float x) {
    return x + -2.03572016;
  }

  float D_4__D_5(float x) {
    return x + -0.40372016;
  }

  float IL_4__IL_4(char a, float x) {
    float e = 0.0;
    if (a == 'A') e = 0.00000000;
    if (a == 'C') e = 0.00000000;
    if (a == 'G') e = 0.00000000;
    if (a == 'U') e = 0.00000000;
    return x + e + -2.81692651;
  }

  float IL_4__IR_4(char a, float x) {
    float e = 0.0;
    if (a == 'A') e = 0.00000000;
    if (a == 'C') e = 0.00000000;
    if (a == 'G') e = 0.00000000;
    if (a == 'U') e = 0.00000000;
    return x + e + -4.31892651;
  }

  float IL_4__MR_5(char a, float x) {
    float e = 0.0;
    if (a == 'A') e = 0.00000000;
    if (a == 'C') e = 0.00000000;
    if (a == 'G') e = 0.00000000;
    if (a == 'U') e = 0.00000000;
    return x + e + -0.61292651;
  }

  float IL_4__D_5(char a, float x) {
    float e = 0.0;
    if (a == 'A') e = 0.00000000;
    if (a == 'C') e = 0.00000000;
    if (a == 'G') e = 0.00000000;
    if (a == 'U') e = 0.00000000;
    return x + e + -2.69792651;
  }

  float IR_4__IR_4(float x, char b) {
    float e = 0.0;
    if (b == 'A') e = 0.00000000;
    if (b == 'C') e = 0.00000000;
    if (b == 'G') e = 0.00000000;
    if (b == 'U') e = 0.00000000;
    return x + e + -1.92536562;
  }

  float IR_4__MR_5(float x, char b) {
    float e = 0.0;
    if (b == 'A') e = 0.00000000;
    if (b == 'C') e = 0.00000000;
    if (b == 'G') e = 0.00000000;
    if (b == 'U') e = 0.00000000;
    return x + e + -0.55436562;
  }

  float IR_4__D_5(float x, char b) {
    float e = 0.0;
    if (b == 'A') e = 0.00000000;
    if (b == 'C') e = 0.00000000;
    if (b == 'G') e = 0.00000000;
    if (b == 'U') e = 0.00000000;
    return x + e + -4.16436562;
  }

  float MR_5__IR_5(float x, char b) {
    float e = 0.0;
    if (b == 'A') e = 1.96985041;
    if (b == 'C') e = -5.53214959;
    if (b == 'G') e = -5.14114959;
    if (b == 'U') e = -4.93114959;
    return x + e + -9.52970807;
  }

  float MR_5__MP_6(float x, char b) {
    float e = 0.0;
    if (b == 'A') e = 1.96985041;
    if (b == 'C') e = -5.53214959;
    if (b == 'G') e = -5.14114959;
    if (b == 'U') e = -4.93114959;
    return x + e + -0.01370807;
  }

  float MR_5__ML_6(float x, char b) {
    float e = 0.0;
    if (b == 'A') e = 1.96985041;
    if (b == 'C') e = -5.53214959;
    if (b == 'G') e = -5.14114959;
    if (b == 'U') e = -4.93114959;
    return x + e + -9.34570807;
  }

  float MR_5__MR_6(float x, char b) {
    float e = 0.0;
    if (b == 'A') e = 1.96985041;
    if (b == 'C') e = -5.53214959;
    if (b == 'G') e = -5.14114959;
    if (b == 'U') e = -4.93114959;
    return x + e + -9.55770807;
  }

  float MR_5__D_6(float x, char b) {
    float e = 0.0;
    if (b == 'A') e = 1.96985041;
    if (b == 'C') e = -5.53214959;
    if (b == 'G') e = -5.14114959;
    if (b == 'U') e = -4.93114959;
    return x + e + -10.44970807;
  }

  float MR_5__EL(Subsequence x, char b) {
    float e = 0.0;
    if (b == 'A') e = 1.96985041;
    if (b == 'C') e = -5.53214959;
    if (b == 'G') e = -5.14114959;
    if (b == 'U') e = -4.93114959;
    return e + -7.78135971 + size(x) * -0.08926734;
  }

  float D_5__IR_5(float x) {
    return x + -5.35201339;
  }

  float D_5__MP_6(float x) {
    return x + -0.70701339;
  }

  float D_5__ML_6(float x) {
    return x + -2.97801339;
  }

  float D_5__MR_6(float x) {
    return x + -4.40901339;
  }

  float D_5__D_6(float x) {
    return x + -2.40401339;
  }

  float IR_5__IR_5(float x, char b) {
    float e = 0.0;
    if (b == 'A') e = 0.00000000;
    if (b == 'C') e = 0.00000000;
    if (b == 'G') e = 0.00000000;
    if (b == 'U') e = 0.00000000;
    return x + e + -2.40826286;
  }

  float IR_5__MP_6(float x, char b) {
    float e = 0.0;
    if (b == 'A') e = 0.00000000;
    if (b == 'C') e = 0.00000000;
    if (b == 'G') e = 0.00000000;
    if (b == 'U') e = 0.00000000;
    return x + e + -0.49626286;
  }

  float IR_5__ML_6(float x, char b) {
    float e = 0.0;
    if (b == 'A') e = 0.00000000;
    if (b == 'C') e = 0.00000000;
    if (b == 'G') e = 0.00000000;
    if (b == 'U') e = 0.00000000;
    return x + e + -5.92026286;
  }

  float IR_5__MR_6(float x, char b) {
    float e = 0.0;
    if (b == 'A') e = 0.00000000;
    if (b == 'C') e = 0.00000000;
    if (b == 'G') e = 0.00000000;
    if (b == 'U') e = 0.00000000;
    return x + e + -4.08726286;
  }

  float IR_5__D_6(float x, char b) {
    float e = 0.0;
    if (b == 'A') e = 0.00000000;
    if (b == 'C') e = 0.00000000;
    if (b == 'G') e = 0.00000000;
    if (b == 'U') e = 0.00000000;
    return x + e + -5.19326286;
  }

  float MP_6__IL_6(char a, float x, char b) {
    float e = 0.0;
    if ((a == 'A') && (b == 'A')) e = -8.58997769;
    if ((a == 'A') && (b == 'C')) e = -6.76397769;
    if ((a == 'A') && (b == 'G')) e = -8.65197769;
    if ((a == 'A') && (b == 'U')) e = 3.96502231;
    if ((a == 'C') && (b == 'A')) e = -8.94697769;
    if ((a == 'C') && (b == 'C')) e = -9.71597769;
    if ((a == 'C') && (b == 'G')) e = -3.97997769;
    if ((a == 'C') && (b == 'U')) e = -7.32897769;
    if ((a == 'G') && (b == 'A')) e = -7.90297769;
    if ((a == 'G') && (b == 'C')) e = -3.36897769;
    if ((a == 'G') && (b == 'G')) e = -7.90297769;
    if ((a == 'G') && (b == 'U')) e = -4.03197769;
    if ((a == 'U') && (b == 'A')) e = -3.58797769;
    if ((a == 'U') && (b == 'C')) e = -8.32397769;
    if ((a == 'U') && (b == 'G')) e = -4.83897769;
    if ((a == 'U') && (b == 'U')) e = -6.85097769;
    return x + e + -10.73039122;
  }

  float MP_6__IR_6(char a, float x, char b) {
    float e = 0.0;
    if ((a == 'A') && (b == 'A')) e = -8.58997769;
    if ((a == 'A') && (b == 'C')) e = -6.76397769;
    if ((a == 'A') && (b == 'G')) e = -8.65197769;
    if ((a == 'A') && (b == 'U')) e = 3.96502231;
    if ((a == 'C') && (b == 'A')) e = -8.94697769;
    if ((a == 'C') && (b == 'C')) e = -9.71597769;
    if ((a == 'C') && (b == 'G')) e = -3.97997769;
    if ((a == 'C') && (b == 'U')) e = -7.32897769;
    if ((a == 'G') && (b == 'A')) e = -7.90297769;
    if ((a == 'G') && (b == 'C')) e = -3.36897769;
    if ((a == 'G') && (b == 'G')) e = -7.90297769;
    if ((a == 'G') && (b == 'U')) e = -4.03197769;
    if ((a == 'U') && (b == 'A')) e = -3.58797769;
    if ((a == 'U') && (b == 'C')) e = -8.32397769;
    if ((a == 'U') && (b == 'G')) e = -4.83897769;
    if ((a == 'U') && (b == 'U')) e = -6.85097769;
    return x + e + -10.66939122;
  }

  float MP_6__MP_7(char a, float x, char b) {
    float e = 0.0;
    if ((a == 'A') && (b == 'A')) e = -8.58997769;
    if ((a == 'A') && (b == 'C')) e = -6.76397769;
    if ((a == 'A') && (b == 'G')) e = -8.65197769;
    if ((a == 'A') && (b == 'U')) e = 3.96502231;
    if ((a == 'C') && (b == 'A')) e = -8.94697769;
    if ((a == 'C') && (b == 'C')) e = -9.71597769;
    if ((a == 'C') && (b == 'G')) e = -3.97997769;
    if ((a == 'C') && (b == 'U')) e = -7.32897769;
    if ((a == 'G') && (b == 'A')) e = -7.90297769;
    if ((a == 'G') && (b == 'C')) e = -3.36897769;
    if ((a == 'G') && (b == 'G')) e = -7.90297769;
    if ((a == 'G') && (b == 'U')) e = -4.03197769;
    if ((a == 'U') && (b == 'A')) e = -3.58797769;
    if ((a == 'U') && (b == 'C')) e = -8.32397769;
    if ((a == 'U') && (b == 'G')) e = -4.83897769;
    if ((a == 'U') && (b == 'U')) e = -6.85097769;
    return x + e + -0.01339122;
  }

  float MP_6__ML_7(char a, float x, char b) {
    float e = 0.0;
    if ((a == 'A') && (b == 'A')) e = -8.58997769;
    if ((a == 'A') && (b == 'C')) e = -6.76397769;
    if ((a == 'A') && (b == 'G')) e = -8.65197769;
    if ((a == 'A') && (b == 'U')) e = 3.96502231;
    if ((a == 'C') && (b == 'A')) e = -8.94697769;
    if ((a == 'C') && (b == 'C')) e = -9.71597769;
    if ((a == 'C') && (b == 'G')) e = -3.97997769;
    if ((a == 'C') && (b == 'U')) e = -7.32897769;
    if ((a == 'G') && (b == 'A')) e = -7.90297769;
    if ((a == 'G') && (b == 'C')) e = -3.36897769;
    if ((a == 'G') && (b == 'G')) e = -7.90297769;
    if ((a == 'G') && (b == 'U')) e = -4.03197769;
    if ((a == 'U') && (b == 'A')) e = -3.58797769;
    if ((a == 'U') && (b == 'C')) e = -8.32397769;
    if ((a == 'U') && (b == 'G')) e = -4.83897769;
    if ((a == 'U') && (b == 'U')) e = -6.85097769;
    return x + e + -9.44639122;
  }

  float MP_6__MR_7(char a, float x, char b) {
    float e = 0.0;
    if ((a == 'A') && (b == 'A')) e = -8.58997769;
    if ((a == 'A') && (b == 'C')) e = -6.76397769;
    if ((a == 'A') && (b == 'G')) e = -8.65197769;
    if ((a == 'A') && (b == 'U')) e = 3.96502231;
    if ((a == 'C') && (b == 'A')) e = -8.94697769;
    if ((a == 'C') && (b == 'C')) e = -9.71597769;
    if ((a == 'C') && (b == 'G')) e = -3.97997769;
    if ((a == 'C') && (b == 'U')) e = -7.32897769;
    if ((a == 'G') && (b == 'A')) e = -7.90297769;
    if ((a == 'G') && (b == 'C')) e = -3.36897769;
    if ((a == 'G') && (b == 'G')) e = -7.90297769;
    if ((a == 'G') && (b == 'U')) e = -4.03197769;
    if ((a == 'U') && (b == 'A')) e = -3.58797769;
    if ((a == 'U') && (b == 'C')) e = -8.32397769;
    if ((a == 'U') && (b == 'G')) e = -4.83897769;
    if ((a == 'U') && (b == 'U')) e = -6.85097769;
    return x + e + -9.72639122;
  }

  float MP_6__D_7(char a, float x, char b) {
    float e = 0.0;
    if ((a == 'A') && (b == 'A')) e = -8.58997769;
    if ((a == 'A') && (b == 'C')) e = -6.76397769;
    if ((a == 'A') && (b == 'G')) e = -8.65197769;
    if ((a == 'A') && (b == 'U')) e = 3.96502231;
    if ((a == 'C') && (b == 'A')) e = -8.94697769;
    if ((a == 'C') && (b == 'C')) e = -9.71597769;
    if ((a == 'C') && (b == 'G')) e = -3.97997769;
    if ((a == 'C') && (b == 'U')) e = -7.32897769;
    if ((a == 'G') && (b == 'A')) e = -7.90297769;
    if ((a == 'G') && (b == 'C')) e = -3.36897769;
    if ((a == 'G') && (b == 'G')) e = -7.90297769;
    if ((a == 'G') && (b == 'U')) e = -4.03197769;
    if ((a == 'U') && (b == 'A')) e = -3.58797769;
    if ((a == 'U') && (b == 'C')) e = -8.32397769;
    if ((a == 'U') && (b == 'G')) e = -4.83897769;
    if ((a == 'U') && (b == 'U')) e = -6.85097769;
    return x + e + -10.12139122;
  }

  float MP_6__EL(char a, Subsequence x, char b) {
    float e = 0.0;
    if ((a == 'A') && (b == 'A')) e = -8.58997769;
    if ((a == 'A') && (b == 'C')) e = -6.76397769;
    if ((a == 'A') && (b == 'G')) e = -8.65197769;
    if ((a == 'A') && (b == 'U')) e = 3.96502231;
    if ((a == 'C') && (b == 'A')) e = -8.94697769;
    if ((a == 'C') && (b == 'C')) e = -9.71597769;
    if ((a == 'C') && (b == 'G')) e = -3.97997769;
    if ((a == 'C') && (b == 'U')) e = -7.32897769;
    if ((a == 'G') && (b == 'A')) e = -7.90297769;
    if ((a == 'G') && (b == 'C')) e = -3.36897769;
    if ((a == 'G') && (b == 'G')) e = -7.90297769;
    if ((a == 'G') && (b == 'U')) e = -4.03197769;
    if ((a == 'U') && (b == 'A')) e = -3.58797769;
    if ((a == 'U') && (b == 'C')) e = -8.32397769;
    if ((a == 'U') && (b == 'G')) e = -4.83897769;
    if ((a == 'U') && (b == 'U')) e = -6.85097769;
    return e + -7.78135971 + size(x) * -0.08926734;
  }

  float ML_6__IL_6(char a, float x) {
    float e = 0.0;
    if (a == 'A') e = 0.66026728;
    if (a == 'C') e = -0.61173272;
    if (a == 'G') e = -0.29273272;
    if (a == 'U') e = -0.07573272;
    return x + e + -6.25018711;
  }

  float ML_6__IR_6(char a, float x) {
    float e = 0.0;
    if (a == 'A') e = 0.66026728;
    if (a == 'C') e = -0.61173272;
    if (a == 'G') e = -0.29273272;
    if (a == 'U') e = -0.07573272;
    return x + e + -6.59618711;
  }

  float ML_6__MP_7(char a, float x) {
    float e = 0.0;
    if (a == 'A') e = 0.66026728;
    if (a == 'C') e = -0.61173272;
    if (a == 'G') e = -0.29273272;
    if (a == 'U') e = -0.07573272;
    return x + e + -1.31018711;
  }

  float ML_6__ML_7(char a, float x) {
    float e = 0.0;
    if (a == 'A') e = 0.66026728;
    if (a == 'C') e = -0.61173272;
    if (a == 'G') e = -0.29273272;
    if (a == 'U') e = -0.07573272;
    return x + e + -1.00518711;
  }

  float ML_6__MR_7(char a, float x) {
    float e = 0.0;
    if (a == 'A') e = 0.66026728;
    if (a == 'C') e = -0.61173272;
    if (a == 'G') e = -0.29273272;
    if (a == 'U') e = -0.07573272;
    return x + e + -6.44618711;
  }

  float ML_6__D_7(char a, float x) {
    float e = 0.0;
    if (a == 'A') e = 0.66026728;
    if (a == 'C') e = -0.61173272;
    if (a == 'G') e = -0.29273272;
    if (a == 'U') e = -0.07573272;
    return x + e + -3.97518711;
  }

  float MR_6__IL_6(float x, char b) {
    float e = 0.0;
    if (b == 'A') e = 0.66026728;
    if (b == 'C') e = -0.61173272;
    if (b == 'G') e = -0.29273272;
    if (b == 'U') e = -0.07573272;
    return x + e + -6.98790919;
  }

  float MR_6__IR_6(float x, char b) {
    float e = 0.0;
    if (b == 'A') e = 0.66026728;
    if (b == 'C') e = -0.61173272;
    if (b == 'G') e = -0.29273272;
    if (b == 'U') e = -0.07573272;
    return x + e + -5.71690919;
  }

  float MR_6__MP_7(float x, char b) {
    float e = 0.0;
    if (b == 'A') e = 0.66026728;
    if (b == 'C') e = -0.61173272;
    if (b == 'G') e = -0.29273272;
    if (b == 'U') e = -0.07573272;
    return x + e + -1.62490919;
  }

  float MR_6__ML_7(float x, char b) {
    float e = 0.0;
    if (b == 'A') e = 0.66026728;
    if (b == 'C') e = -0.61173272;
    if (b == 'G') e = -0.29273272;
    if (b == 'U') e = -0.07573272;
    return x + e + -5.69490919;
  }

  float MR_6__MR_7(float x, char b) {
    float e = 0.0;
    if (b == 'A') e = 0.66026728;
    if (b == 'C') e = -0.61173272;
    if (b == 'G') e = -0.29273272;
    if (b == 'U') e = -0.07573272;
    return x + e + -0.82890919;
  }

  float MR_6__D_7(float x, char b) {
    float e = 0.0;
    if (b == 'A') e = 0.66026728;
    if (b == 'C') e = -0.61173272;
    if (b == 'G') e = -0.29273272;
    if (b == 'U') e = -0.07573272;
    return x + e + -3.90790919;
  }

  float D_6__IL_6(float x) {
    return x + -9.04916484;
  }

  float D_6__IR_6(float x) {
    return x + -7.74716484;
  }

  float D_6__MP_7(float x) {
    return x + -3.54416484;
  }

  float D_6__ML_7(float x) {
    return x + -4.22616484;
  }

  float D_6__MR_7(float x) {
    return x + -4.24416484;
  }

  float D_6__D_7(float x) {
    return x + -0.31916484;
  }

  float IL_6__IL_6(char a, float x) {
    float e = 0.0;
    if (a == 'A') e = 0.00000000;
    if (a == 'C') e = 0.00000000;
    if (a == 'G') e = 0.00000000;
    if (a == 'U') e = 0.00000000;
    return x + e + -2.57923676;
  }

  float IL_6__IR_6(char a, float x) {
    float e = 0.0;
    if (a == 'A') e = 0.00000000;
    if (a == 'C') e = 0.00000000;
    if (a == 'G') e = 0.00000000;
    if (a == 'U') e = 0.00000000;
    return x + e + -2.84223676;
  }

  float IL_6__MP_7(char a, float x) {
    float e = 0.0;
    if (a == 'A') e = 0.00000000;
    if (a == 'C') e = 0.00000000;
    if (a == 'G') e = 0.00000000;
    if (a == 'U') e = 0.00000000;
    return x + e + -0.76023676;
  }

  float IL_6__ML_7(char a, float x) {
    float e = 0.0;
    if (a == 'A') e = 0.00000000;
    if (a == 'C') e = 0.00000000;
    if (a == 'G') e = 0.00000000;
    if (a == 'U') e = 0.00000000;
    return x + e + -4.49723676;
  }

  float IL_6__MR_7(char a, float x) {
    float e = 0.0;
    if (a == 'A') e = 0.00000000;
    if (a == 'C') e = 0.00000000;
    if (a == 'G') e = 0.00000000;
    if (a == 'U') e = 0.00000000;
    return x + e + -5.27423676;
  }

  float IL_6__D_7(char a, float x) {
    float e = 0.0;
    if (a == 'A') e = 0.00000000;
    if (a == 'C') e = 0.00000000;
    if (a == 'G') e = 0.00000000;
    if (a == 'U') e = 0.00000000;
    return x + e + -4.93423676;
  }

  float IR_6__IR_6(float x, char b) {
    float e = 0.0;
    if (b == 'A') e = 0.00000000;
    if (b == 'C') e = 0.00000000;
    if (b == 'G') e = 0.00000000;
    if (b == 'U') e = 0.00000000;
    return x + e + -2.40826286;
  }

  float IR_6__MP_7(float x, char b) {
    float e = 0.0;
    if (b == 'A') e = 0.00000000;
    if (b == 'C') e = 0.00000000;
    if (b == 'G') e = 0.00000000;
    if (b == 'U') e = 0.00000000;
    return x + e + -0.49626286;
  }

  float IR_6__ML_7(float x, char b) {
    float e = 0.0;
    if (b == 'A') e = 0.00000000;
    if (b == 'C') e = 0.00000000;
    if (b == 'G') e = 0.00000000;
    if (b == 'U') e = 0.00000000;
    return x + e + -5.92026286;
  }

  float IR_6__MR_7(float x, char b) {
    float e = 0.0;
    if (b == 'A') e = 0.00000000;
    if (b == 'C') e = 0.00000000;
    if (b == 'G') e = 0.00000000;
    if (b == 'U') e = 0.00000000;
    return x + e + -4.08726286;
  }

  float IR_6__D_7(float x, char b) {
    float e = 0.0;
    if (b == 'A') e = 0.00000000;
    if (b == 'C') e = 0.00000000;
    if (b == 'G') e = 0.00000000;
    if (b == 'U') e = 0.00000000;
    return x + e + -5.19326286;
  }

  float MP_7__IL_7(char a, float x, char b) {
    float e = 0.0;
    if ((a == 'A') && (b == 'A')) e = -6.64440026;
    if ((a == 'A') && (b == 'C')) e = -6.93840026;
    if ((a == 'A') && (b == 'G')) e = 0.77559974;
    if ((a == 'A') && (b == 'U')) e = -2.64940026;
    if ((a == 'C') && (b == 'A')) e = -4.60140026;
    if ((a == 'C') && (b == 'C')) e = -5.51040026;
    if ((a == 'C') && (b == 'G')) e = 3.70159974;
    if ((a == 'C') && (b == 'U')) e = -5.60940026;
    if ((a == 'G') && (b == 'A')) e = -8.22640026;
    if ((a == 'G') && (b == 'C')) e = -2.60940026;
    if ((a == 'G') && (b == 'G')) e = -4.92340026;
    if ((a == 'G') && (b == 'U')) e = -4.41340026;
    if ((a == 'U') && (b == 'A')) e = -0.97640026;
    if ((a == 'U') && (b == 'C')) e = -7.34940026;
    if ((a == 'U') && (b == 'G')) e = -2.08240026;
    if ((a == 'U') && (b == 'U')) e = -5.72540026;
    return x + e + -10.73039122;
  }

  float MP_7__IR_7(char a, float x, char b) {
    float e = 0.0;
    if ((a == 'A') && (b == 'A')) e = -6.64440026;
    if ((a == 'A') && (b == 'C')) e = -6.93840026;
    if ((a == 'A') && (b == 'G')) e = 0.77559974;
    if ((a == 'A') && (b == 'U')) e = -2.64940026;
    if ((a == 'C') && (b == 'A')) e = -4.60140026;
    if ((a == 'C') && (b == 'C')) e = -5.51040026;
    if ((a == 'C') && (b == 'G')) e = 3.70159974;
    if ((a == 'C') && (b == 'U')) e = -5.60940026;
    if ((a == 'G') && (b == 'A')) e = -8.22640026;
    if ((a == 'G') && (b == 'C')) e = -2.60940026;
    if ((a == 'G') && (b == 'G')) e = -4.92340026;
    if ((a == 'G') && (b == 'U')) e = -4.41340026;
    if ((a == 'U') && (b == 'A')) e = -0.97640026;
    if ((a == 'U') && (b == 'C')) e = -7.34940026;
    if ((a == 'U') && (b == 'G')) e = -2.08240026;
    if ((a == 'U') && (b == 'U')) e = -5.72540026;
    return x + e + -10.66939122;
  }

  float MP_7__MP_8(char a, float x, char b) {
    float e = 0.0;
    if ((a == 'A') && (b == 'A')) e = -6.64440026;
    if ((a == 'A') && (b == 'C')) e = -6.93840026;
    if ((a == 'A') && (b == 'G')) e = 0.77559974;
    if ((a == 'A') && (b == 'U')) e = -2.64940026;
    if ((a == 'C') && (b == 'A')) e = -4.60140026;
    if ((a == 'C') && (b == 'C')) e = -5.51040026;
    if ((a == 'C') && (b == 'G')) e = 3.70159974;
    if ((a == 'C') && (b == 'U')) e = -5.60940026;
    if ((a == 'G') && (b == 'A')) e = -8.22640026;
    if ((a == 'G') && (b == 'C')) e = -2.60940026;
    if ((a == 'G') && (b == 'G')) e = -4.92340026;
    if ((a == 'G') && (b == 'U')) e = -4.41340026;
    if ((a == 'U') && (b == 'A')) e = -0.97640026;
    if ((a == 'U') && (b == 'C')) e = -7.34940026;
    if ((a == 'U') && (b == 'G')) e = -2.08240026;
    if ((a == 'U') && (b == 'U')) e = -5.72540026;
    return x + e + -0.01339122;
  }

  float MP_7__ML_8(char a, float x, char b) {
    float e = 0.0;
    if ((a == 'A') && (b == 'A')) e = -6.64440026;
    if ((a == 'A') && (b == 'C')) e = -6.93840026;
    if ((a == 'A') && (b == 'G')) e = 0.77559974;
    if ((a == 'A') && (b == 'U')) e = -2.64940026;
    if ((a == 'C') && (b == 'A')) e = -4.60140026;
    if ((a == 'C') && (b == 'C')) e = -5.51040026;
    if ((a == 'C') && (b == 'G')) e = 3.70159974;
    if ((a == 'C') && (b == 'U')) e = -5.60940026;
    if ((a == 'G') && (b == 'A')) e = -8.22640026;
    if ((a == 'G') && (b == 'C')) e = -2.60940026;
    if ((a == 'G') && (b == 'G')) e = -4.92340026;
    if ((a == 'G') && (b == 'U')) e = -4.41340026;
    if ((a == 'U') && (b == 'A')) e = -0.97640026;
    if ((a == 'U') && (b == 'C')) e = -7.34940026;
    if ((a == 'U') && (b == 'G')) e = -2.08240026;
    if ((a == 'U') && (b == 'U')) e = -5.72540026;
    return x + e + -9.44639122;
  }

  float MP_7__MR_8(char a, float x, char b) {
    float e = 0.0;
    if ((a == 'A') && (b == 'A')) e = -6.64440026;
    if ((a == 'A') && (b == 'C')) e = -6.93840026;
    if ((a == 'A') && (b == 'G')) e = 0.77559974;
    if ((a == 'A') && (b == 'U')) e = -2.64940026;
    if ((a == 'C') && (b == 'A')) e = -4.60140026;
    if ((a == 'C') && (b == 'C')) e = -5.51040026;
    if ((a == 'C') && (b == 'G')) e = 3.70159974;
    if ((a == 'C') && (b == 'U')) e = -5.60940026;
    if ((a == 'G') && (b == 'A')) e = -8.22640026;
    if ((a == 'G') && (b == 'C')) e = -2.60940026;
    if ((a == 'G') && (b == 'G')) e = -4.92340026;
    if ((a == 'G') && (b == 'U')) e = -4.41340026;
    if ((a == 'U') && (b == 'A')) e = -0.97640026;
    if ((a == 'U') && (b == 'C')) e = -7.34940026;
    if ((a == 'U') && (b == 'G')) e = -2.08240026;
    if ((a == 'U') && (b == 'U')) e = -5.72540026;
    return x + e + -9.72639122;
  }

  float MP_7__D_8(char a, float x, char b) {
    float e = 0.0;
    if ((a == 'A') && (b == 'A')) e = -6.64440026;
    if ((a == 'A') && (b == 'C')) e = -6.93840026;
    if ((a == 'A') && (b == 'G')) e = 0.77559974;
    if ((a == 'A') && (b == 'U')) e = -2.64940026;
    if ((a == 'C') && (b == 'A')) e = -4.60140026;
    if ((a == 'C') && (b == 'C')) e = -5.51040026;
    if ((a == 'C') && (b == 'G')) e = 3.70159974;
    if ((a == 'C') && (b == 'U')) e = -5.60940026;
    if ((a == 'G') && (b == 'A')) e = -8.22640026;
    if ((a == 'G') && (b == 'C')) e = -2.60940026;
    if ((a == 'G') && (b == 'G')) e = -4.92340026;
    if ((a == 'G') && (b == 'U')) e = -4.41340026;
    if ((a == 'U') && (b == 'A')) e = -0.97640026;
    if ((a == 'U') && (b == 'C')) e = -7.34940026;
    if ((a == 'U') && (b == 'G')) e = -2.08240026;
    if ((a == 'U') && (b == 'U')) e = -5.72540026;
    return x + e + -10.12139122;
  }

  float MP_7__EL(char a, Subsequence x, char b) {
    float e = 0.0;
    if ((a == 'A') && (b == 'A')) e = -6.64440026;
    if ((a == 'A') && (b == 'C')) e = -6.93840026;
    if ((a == 'A') && (b == 'G')) e = 0.77559974;
    if ((a == 'A') && (b == 'U')) e = -2.64940026;
    if ((a == 'C') && (b == 'A')) e = -4.60140026;
    if ((a == 'C') && (b == 'C')) e = -5.51040026;
    if ((a == 'C') && (b == 'G')) e = 3.70159974;
    if ((a == 'C') && (b == 'U')) e = -5.60940026;
    if ((a == 'G') && (b == 'A')) e = -8.22640026;
    if ((a == 'G') && (b == 'C')) e = -2.60940026;
    if ((a == 'G') && (b == 'G')) e = -4.92340026;
    if ((a == 'G') && (b == 'U')) e = -4.41340026;
    if ((a == 'U') && (b == 'A')) e = -0.97640026;
    if ((a == 'U') && (b == 'C')) e = -7.34940026;
    if ((a == 'U') && (b == 'G')) e = -2.08240026;
    if ((a == 'U') && (b == 'U')) e = -5.72540026;
    return e + -7.78135971 + size(x) * -0.08926734;
  }

  float ML_7__IL_7(char a, float x) {
    float e = 0.0;
    if (a == 'A') e = 0.66026728;
    if (a == 'C') e = -0.61173272;
    if (a == 'G') e = -0.29273272;
    if (a == 'U') e = -0.07573272;
    return x + e + -6.25018711;
  }

  float ML_7__IR_7(char a, float x) {
    float e = 0.0;
    if (a == 'A') e = 0.66026728;
    if (a == 'C') e = -0.61173272;
    if (a == 'G') e = -0.29273272;
    if (a == 'U') e = -0.07573272;
    return x + e + -6.59618711;
  }

  float ML_7__MP_8(char a, float x) {
    float e = 0.0;
    if (a == 'A') e = 0.66026728;
    if (a == 'C') e = -0.61173272;
    if (a == 'G') e = -0.29273272;
    if (a == 'U') e = -0.07573272;
    return x + e + -1.31018711;
  }

  float ML_7__ML_8(char a, float x) {
    float e = 0.0;
    if (a == 'A') e = 0.66026728;
    if (a == 'C') e = -0.61173272;
    if (a == 'G') e = -0.29273272;
    if (a == 'U') e = -0.07573272;
    return x + e + -1.00518711;
  }

  float ML_7__MR_8(char a, float x) {
    float e = 0.0;
    if (a == 'A') e = 0.66026728;
    if (a == 'C') e = -0.61173272;
    if (a == 'G') e = -0.29273272;
    if (a == 'U') e = -0.07573272;
    return x + e + -6.44618711;
  }

  float ML_7__D_8(char a, float x) {
    float e = 0.0;
    if (a == 'A') e = 0.66026728;
    if (a == 'C') e = -0.61173272;
    if (a == 'G') e = -0.29273272;
    if (a == 'U') e = -0.07573272;
    return x + e + -3.97518711;
  }

  float MR_7__IL_7(float x, char b) {
    float e = 0.0;
    if (b == 'A') e = 0.66026728;
    if (b == 'C') e = -0.61173272;
    if (b == 'G') e = -0.29273272;
    if (b == 'U') e = -0.07573272;
    return x + e + -6.98790919;
  }

  float MR_7__IR_7(float x, char b) {
    float e = 0.0;
    if (b == 'A') e = 0.66026728;
    if (b == 'C') e = -0.61173272;
    if (b == 'G') e = -0.29273272;
    if (b == 'U') e = -0.07573272;
    return x + e + -5.71690919;
  }

  float MR_7__MP_8(float x, char b) {
    float e = 0.0;
    if (b == 'A') e = 0.66026728;
    if (b == 'C') e = -0.61173272;
    if (b == 'G') e = -0.29273272;
    if (b == 'U') e = -0.07573272;
    return x + e + -1.62490919;
  }

  float MR_7__ML_8(float x, char b) {
    float e = 0.0;
    if (b == 'A') e = 0.66026728;
    if (b == 'C') e = -0.61173272;
    if (b == 'G') e = -0.29273272;
    if (b == 'U') e = -0.07573272;
    return x + e + -5.69490919;
  }

  float MR_7__MR_8(float x, char b) {
    float e = 0.0;
    if (b == 'A') e = 0.66026728;
    if (b == 'C') e = -0.61173272;
    if (b == 'G') e = -0.29273272;
    if (b == 'U') e = -0.07573272;
    return x + e + -0.82890919;
  }

  float MR_7__D_8(float x, char b) {
    float e = 0.0;
    if (b == 'A') e = 0.66026728;
    if (b == 'C') e = -0.61173272;
    if (b == 'G') e = -0.29273272;
    if (b == 'U') e = -0.07573272;
    return x + e + -3.90790919;
  }

  float D_7__IL_7(float x) {
    return x + -9.04916484;
  }

  float D_7__IR_7(float x) {
    return x + -7.74716484;
  }

  float D_7__MP_8(float x) {
    return x + -3.54416484;
  }

  float D_7__ML_8(float x) {
    return x + -4.22616484;
  }

  float D_7__MR_8(float x) {
    return x + -4.24416484;
  }

  float D_7__D_8(float x) {
    return x + -0.31916484;
  }

  float IL_7__IL_7(char a, float x) {
    float e = 0.0;
    if (a == 'A') e = 0.00000000;
    if (a == 'C') e = 0.00000000;
    if (a == 'G') e = 0.00000000;
    if (a == 'U') e = 0.00000000;
    return x + e + -2.57923676;
  }

  float IL_7__IR_7(char a, float x) {
    float e = 0.0;
    if (a == 'A') e = 0.00000000;
    if (a == 'C') e = 0.00000000;
    if (a == 'G') e = 0.00000000;
    if (a == 'U') e = 0.00000000;
    return x + e + -2.84223676;
  }

  float IL_7__MP_8(char a, float x) {
    float e = 0.0;
    if (a == 'A') e = 0.00000000;
    if (a == 'C') e = 0.00000000;
    if (a == 'G') e = 0.00000000;
    if (a == 'U') e = 0.00000000;
    return x + e + -0.76023676;
  }

  float IL_7__ML_8(char a, float x) {
    float e = 0.0;
    if (a == 'A') e = 0.00000000;
    if (a == 'C') e = 0.00000000;
    if (a == 'G') e = 0.00000000;
    if (a == 'U') e = 0.00000000;
    return x + e + -4.49723676;
  }

  float IL_7__MR_8(char a, float x) {
    float e = 0.0;
    if (a == 'A') e = 0.00000000;
    if (a == 'C') e = 0.00000000;
    if (a == 'G') e = 0.00000000;
    if (a == 'U') e = 0.00000000;
    return x + e + -5.27423676;
  }

  float IL_7__D_8(char a, float x) {
    float e = 0.0;
    if (a == 'A') e = 0.00000000;
    if (a == 'C') e = 0.00000000;
    if (a == 'G') e = 0.00000000;
    if (a == 'U') e = 0.00000000;
    return x + e + -4.93423676;
  }

  float IR_7__IR_7(float x, char b) {
    float e = 0.0;
    if (b == 'A') e = 0.00000000;
    if (b == 'C') e = 0.00000000;
    if (b == 'G') e = 0.00000000;
    if (b == 'U') e = 0.00000000;
    return x + e + -2.40826286;
  }

  float IR_7__MP_8(float x, char b) {
    float e = 0.0;
    if (b == 'A') e = 0.00000000;
    if (b == 'C') e = 0.00000000;
    if (b == 'G') e = 0.00000000;
    if (b == 'U') e = 0.00000000;
    return x + e + -0.49626286;
  }

  float IR_7__ML_8(float x, char b) {
    float e = 0.0;
    if (b == 'A') e = 0.00000000;
    if (b == 'C') e = 0.00000000;
    if (b == 'G') e = 0.00000000;
    if (b == 'U') e = 0.00000000;
    return x + e + -5.92026286;
  }

  float IR_7__MR_8(float x, char b) {
    float e = 0.0;
    if (b == 'A') e = 0.00000000;
    if (b == 'C') e = 0.00000000;
    if (b == 'G') e = 0.00000000;
    if (b == 'U') e = 0.00000000;
    return x + e + -4.08726286;
  }

  float IR_7__D_8(float x, char b) {
    float e = 0.0;
    if (b == 'A') e = 0.00000000;
    if (b == 'C') e = 0.00000000;
    if (b == 'G') e = 0.00000000;
    if (b == 'U') e = 0.00000000;
    return x + e + -5.19326286;
  }

  float MP_8__IL_8(char a, float x, char b) {
    float e = 0.0;
    if ((a == 'A') && (b == 'A')) e = -4.15318697;
    if ((a == 'A') && (b == 'C')) e = -5.21418697;
    if ((a == 'A') && (b == 'G')) e = -4.93018697;
    if ((a == 'A') && (b == 'U')) e = -2.71218697;
    if ((a == 'C') && (b == 'A')) e = 2.28081303;
    if ((a == 'C') && (b == 'C')) e = -5.91618697;
    if ((a == 'C') && (b == 'G')) e = -2.13618697;
    if ((a == 'C') && (b == 'U')) e = -5.27118697;
    if ((a == 'G') && (b == 'A')) e = -4.68218697;
    if ((a == 'G') && (b == 'C')) e = -3.27018697;
    if ((a == 'G') && (b == 'G')) e = -5.54418697;
    if ((a == 'G') && (b == 'U')) e = -3.73118697;
    if ((a == 'U') && (b == 'A')) e = 3.33681303;
    if ((a == 'U') && (b == 'C')) e = -4.71018697;
    if ((a == 'U') && (b == 'G')) e = -2.61618697;
    if ((a == 'U') && (b == 'U')) e = -4.11618697;
    return x + e + -9.63669876;
  }

  float MP_8__IR_8(char a, float x, char b) {
    float e = 0.0;
    if ((a == 'A') && (b == 'A')) e = -4.15318697;
    if ((a == 'A') && (b == 'C')) e = -5.21418697;
    if ((a == 'A') && (b == 'G')) e = -4.93018697;
    if ((a == 'A') && (b == 'U')) e = -2.71218697;
    if ((a == 'C') && (b == 'A')) e = 2.28081303;
    if ((a == 'C') && (b == 'C')) e = -5.91618697;
    if ((a == 'C') && (b == 'G')) e = -2.13618697;
    if ((a == 'C') && (b == 'U')) e = -5.27118697;
    if ((a == 'G') && (b == 'A')) e = -4.68218697;
    if ((a == 'G') && (b == 'C')) e = -3.27018697;
    if ((a == 'G') && (b == 'G')) e = -5.54418697;
    if ((a == 'G') && (b == 'U')) e = -3.73118697;
    if ((a == 'U') && (b == 'A')) e = 3.33681303;
    if ((a == 'U') && (b == 'C')) e = -4.71018697;
    if ((a == 'U') && (b == 'G')) e = -2.61618697;
    if ((a == 'U') && (b == 'U')) e = -4.11618697;
    return x + e + -9.84369876;
  }

  float MP_8__ML_9(char a, float x, char b) {
    float e = 0.0;
    if ((a == 'A') && (b == 'A')) e = -4.15318697;
    if ((a == 'A') && (b == 'C')) e = -5.21418697;
    if ((a == 'A') && (b == 'G')) e = -4.93018697;
    if ((a == 'A') && (b == 'U')) e = -2.71218697;
    if ((a == 'C') && (b == 'A')) e = 2.28081303;
    if ((a == 'C') && (b == 'C')) e = -5.91618697;
    if ((a == 'C') && (b == 'G')) e = -2.13618697;
    if ((a == 'C') && (b == 'U')) e = -5.27118697;
    if ((a == 'G') && (b == 'A')) e = -4.68218697;
    if ((a == 'G') && (b == 'C')) e = -3.27018697;
    if ((a == 'G') && (b == 'G')) e = -5.54418697;
    if ((a == 'G') && (b == 'U')) e = -3.73118697;
    if ((a == 'U') && (b == 'A')) e = 3.33681303;
    if ((a == 'U') && (b == 'C')) e = -4.71018697;
    if ((a == 'U') && (b == 'G')) e = -2.61618697;
    if ((a == 'U') && (b == 'U')) e = -4.11618697;
    return x + e + -0.01469876;
  }

  float MP_8__D_9(char a, float x, char b) {
    float e = 0.0;
    if ((a == 'A') && (b == 'A')) e = -4.15318697;
    if ((a == 'A') && (b == 'C')) e = -5.21418697;
    if ((a == 'A') && (b == 'G')) e = -4.93018697;
    if ((a == 'A') && (b == 'U')) e = -2.71218697;
    if ((a == 'C') && (b == 'A')) e = 2.28081303;
    if ((a == 'C') && (b == 'C')) e = -5.91618697;
    if ((a == 'C') && (b == 'G')) e = -2.13618697;
    if ((a == 'C') && (b == 'U')) e = -5.27118697;
    if ((a == 'G') && (b == 'A')) e = -4.68218697;
    if ((a == 'G') && (b == 'C')) e = -3.27018697;
    if ((a == 'G') && (b == 'G')) e = -5.54418697;
    if ((a == 'G') && (b == 'U')) e = -3.73118697;
    if ((a == 'U') && (b == 'A')) e = 3.33681303;
    if ((a == 'U') && (b == 'C')) e = -4.71018697;
    if ((a == 'U') && (b == 'G')) e = -2.61618697;
    if ((a == 'U') && (b == 'U')) e = -4.11618697;
    return x + e + -8.25769876;
  }

  float MP_8__EL(char a, Subsequence x, char b) {
    float e = 0.0;
    if ((a == 'A') && (b == 'A')) e = -4.15318697;
    if ((a == 'A') && (b == 'C')) e = -5.21418697;
    if ((a == 'A') && (b == 'G')) e = -4.93018697;
    if ((a == 'A') && (b == 'U')) e = -2.71218697;
    if ((a == 'C') && (b == 'A')) e = 2.28081303;
    if ((a == 'C') && (b == 'C')) e = -5.91618697;
    if ((a == 'C') && (b == 'G')) e = -2.13618697;
    if ((a == 'C') && (b == 'U')) e = -5.27118697;
    if ((a == 'G') && (b == 'A')) e = -4.68218697;
    if ((a == 'G') && (b == 'C')) e = -3.27018697;
    if ((a == 'G') && (b == 'G')) e = -5.54418697;
    if ((a == 'G') && (b == 'U')) e = -3.73118697;
    if ((a == 'U') && (b == 'A')) e = 3.33681303;
    if ((a == 'U') && (b == 'C')) e = -4.71018697;
    if ((a == 'U') && (b == 'G')) e = -2.61618697;
    if ((a == 'U') && (b == 'U')) e = -4.11618697;
    return e + -7.78135971 + size(x) * -0.08926734;
  }

  float ML_8__IL_8(char a, float x) {
    float e = 0.0;
    if (a == 'A') e = 0.66026728;
    if (a == 'C') e = -0.61173272;
    if (a == 'G') e = -0.29273272;
    if (a == 'U') e = -0.07573272;
    return x + e + -3.75782601;
  }

  float ML_8__IR_8(char a, float x) {
    float e = 0.0;
    if (a == 'A') e = 0.66026728;
    if (a == 'C') e = -0.61173272;
    if (a == 'G') e = -0.29273272;
    if (a == 'U') e = -0.07573272;
    return x + e + -3.93982601;
  }

  float ML_8__ML_9(char a, float x) {
    float e = 0.0;
    if (a == 'A') e = 0.66026728;
    if (a == 'C') e = -0.61173272;
    if (a == 'G') e = -0.29273272;
    if (a == 'U') e = -0.07573272;
    return x + e + -0.50682601;
  }

  float ML_8__D_9(char a, float x) {
    float e = 0.0;
    if (a == 'A') e = 0.66026728;
    if (a == 'C') e = -0.61173272;
    if (a == 'G') e = -0.29273272;
    if (a == 'U') e = -0.07573272;
    return x + e + -2.66982601;
  }

  float MR_8__IL_8(float x, char b) {
    float e = 0.0;
    if (b == 'A') e = 0.66026728;
    if (b == 'C') e = -0.61173272;
    if (b == 'G') e = -0.29273272;
    if (b == 'U') e = -0.07573272;
    return x + e + -4.80922395;
  }

  float MR_8__IR_8(float x, char b) {
    float e = 0.0;
    if (b == 'A') e = 0.66026728;
    if (b == 'C') e = -0.61173272;
    if (b == 'G') e = -0.29273272;
    if (b == 'U') e = -0.07573272;
    return x + e + -3.83822395;
  }

  float MR_8__ML_9(float x, char b) {
    float e = 0.0;
    if (b == 'A') e = 0.66026728;
    if (b == 'C') e = -0.61173272;
    if (b == 'G') e = -0.29273272;
    if (b == 'U') e = -0.07573272;
    return x + e + -1.70622395;
  }

  float MR_8__D_9(float x, char b) {
    float e = 0.0;
    if (b == 'A') e = 0.66026728;
    if (b == 'C') e = -0.61173272;
    if (b == 'G') e = -0.29273272;
    if (b == 'U') e = -0.07573272;
    return x + e + -0.76622395;
  }

  float D_8__IL_8(float x) {
    return x + -4.56819763;
  }

  float D_8__IR_8(float x) {
    return x + -4.25019763;
  }

  float D_8__ML_9(float x) {
    return x + -2.26519763;
  }

  float D_8__D_9(float x) {
    return x + -0.52019763;
  }

  float IL_8__IL_8(char a, float x) {
    float e = 0.0;
    if (a == 'A') e = 0.00000000;
    if (a == 'C') e = 0.00000000;
    if (a == 'G') e = 0.00000000;
    if (a == 'U') e = 0.00000000;
    return x + e + -1.68596078;
  }

  float IL_8__IR_8(char a, float x) {
    float e = 0.0;
    if (a == 'A') e = 0.00000000;
    if (a == 'C') e = 0.00000000;
    if (a == 'G') e = 0.00000000;
    if (a == 'U') e = 0.00000000;
    return x + e + -2.36896078;
  }

  float IL_8__ML_9(char a, float x) {
    float e = 0.0;
    if (a == 'A') e = 0.00000000;
    if (a == 'C') e = 0.00000000;
    if (a == 'G') e = 0.00000000;
    if (a == 'U') e = 0.00000000;
    return x + e + -1.11696078;
  }

  float IL_8__D_9(char a, float x) {
    float e = 0.0;
    if (a == 'A') e = 0.00000000;
    if (a == 'C') e = 0.00000000;
    if (a == 'G') e = 0.00000000;
    if (a == 'U') e = 0.00000000;
    return x + e + -4.85496078;
  }

  float IR_8__IR_8(float x, char b) {
    float e = 0.0;
    if (b == 'A') e = 0.00000000;
    if (b == 'C') e = 0.00000000;
    if (b == 'G') e = 0.00000000;
    if (b == 'U') e = 0.00000000;
    return x + e + -1.44177497;
  }

  float IR_8__ML_9(float x, char b) {
    float e = 0.0;
    if (b == 'A') e = 0.00000000;
    if (b == 'C') e = 0.00000000;
    if (b == 'G') e = 0.00000000;
    if (b == 'U') e = 0.00000000;
    return x + e + -0.79777497;
  }

  float IR_8__D_9(float x, char b) {
    float e = 0.0;
    if (b == 'A') e = 0.00000000;
    if (b == 'C') e = 0.00000000;
    if (b == 'G') e = 0.00000000;
    if (b == 'U') e = 0.00000000;
    return x + e + -4.14177497;
  }

  float ML_9__IL_9(char a, float x) {
    float e = 0.0;
    if (a == 'A') e = -3.79892278;
    if (a == 'C') e = -5.23592278;
    if (a == 'G') e = 1.94807722;
    if (a == 'U') e = -4.53892278;
    return x + e + -10.56294027;
  }

  float ML_9__ML_10(char a, float x) {
    float e = 0.0;
    if (a == 'A') e = -3.79892278;
    if (a == 'C') e = -5.23592278;
    if (a == 'G') e = 1.94807722;
    if (a == 'U') e = -4.53892278;
    return x + e + -0.00994027;
  }

  float ML_9__D_10(char a, float x) {
    float e = 0.0;
    if (a == 'A') e = -3.79892278;
    if (a == 'C') e = -5.23592278;
    if (a == 'G') e = 1.94807722;
    if (a == 'U') e = -4.53892278;
    return x + e + -9.21694027;
  }

  float ML_9__EL(char a, Subsequence x) {
    float e = 0.0;
    if (a == 'A') e = -3.79892278;
    if (a == 'C') e = -5.23592278;
    if (a == 'G') e = 1.94807722;
    if (a == 'U') e = -4.53892278;
    return e + -7.78135971 + size(x) * -0.08926734;
  }

  float D_9__IL_9(float x) {
    return x + -6.17386846;
  }

  float D_9__ML_10(float x) {
    return x + -1.68686846;
  }

  float D_9__D_10(float x) {
    return x + -0.56586846;
  }

  float IL_9__IL_9(char a, float x) {
    float e = 0.0;
    if (a == 'A') e = 0.00000000;
    if (a == 'C') e = 0.00000000;
    if (a == 'G') e = 0.00000000;
    if (a == 'U') e = 0.00000000;
    return x + e + -1.44177497;
  }

  float IL_9__ML_10(char a, float x) {
    float e = 0.0;
    if (a == 'A') e = 0.00000000;
    if (a == 'C') e = 0.00000000;
    if (a == 'G') e = 0.00000000;
    if (a == 'U') e = 0.00000000;
    return x + e + -0.79777497;
  }

  float IL_9__D_10(char a, float x) {
    float e = 0.0;
    if (a == 'A') e = 0.00000000;
    if (a == 'C') e = 0.00000000;
    if (a == 'G') e = 0.00000000;
    if (a == 'U') e = 0.00000000;
    return x + e + -4.14177497;
  }

  float ML_10__IL_10(char a, float x) {
    float e = 0.0;
    if (a == 'A') e = -3.79892278;
    if (a == 'C') e = -5.23592278;
    if (a == 'G') e = 1.94807722;
    if (a == 'U') e = -4.53892278;
    return x + e + -10.56294027;
  }

  float ML_10__ML_11(char a, float x) {
    float e = 0.0;
    if (a == 'A') e = -3.79892278;
    if (a == 'C') e = -5.23592278;
    if (a == 'G') e = 1.94807722;
    if (a == 'U') e = -4.53892278;
    return x + e + -0.00994027;
  }

  float ML_10__D_11(char a, float x) {
    float e = 0.0;
    if (a == 'A') e = -3.79892278;
    if (a == 'C') e = -5.23592278;
    if (a == 'G') e = 1.94807722;
    if (a == 'U') e = -4.53892278;
    return x + e + -9.21694027;
  }

  float ML_10__EL(char a, Subsequence x) {
    float e = 0.0;
    if (a == 'A') e = -3.79892278;
    if (a == 'C') e = -5.23592278;
    if (a == 'G') e = 1.94807722;
    if (a == 'U') e = -4.53892278;
    return e + -7.78135971 + size(x) * -0.08926734;
  }

  float D_10__IL_10(float x) {
    return x + -6.17386846;
  }

  float D_10__ML_11(float x) {
    return x + -1.68686846;
  }

  float D_10__D_11(float x) {
    return x + -0.56586846;
  }

  float IL_10__IL_10(char a, float x) {
    float e = 0.0;
    if (a == 'A') e = 0.00000000;
    if (a == 'C') e = 0.00000000;
    if (a == 'G') e = 0.00000000;
    if (a == 'U') e = 0.00000000;
    return x + e + -1.44177497;
  }

  float IL_10__ML_11(char a, float x) {
    float e = 0.0;
    if (a == 'A') e = 0.00000000;
    if (a == 'C') e = 0.00000000;
    if (a == 'G') e = 0.00000000;
    if (a == 'U') e = 0.00000000;
    return x + e + -0.79777497;
  }

  float IL_10__D_11(char a, float x) {
    float e = 0.0;
    if (a == 'A') e = 0.00000000;
    if (a == 'C') e = 0.00000000;
    if (a == 'G') e = 0.00000000;
    if (a == 'U') e = 0.00000000;
    return x + e + -4.14177497;
  }

  float ML_11__IL_11(char a, float x) {
    float e = 0.0;
    if (a == 'A') e = -4.33280170;
    if (a == 'C') e = -4.03380170;
    if (a == 'G') e = -5.03980170;
    if (a == 'U') e = 1.94819830;
    return x + e + -10.56294027;
  }

  float ML_11__ML_12(char a, float x) {
    float e = 0.0;
    if (a == 'A') e = -4.33280170;
    if (a == 'C') e = -4.03380170;
    if (a == 'G') e = -5.03980170;
    if (a == 'U') e = 1.94819830;
    return x + e + -0.00994027;
  }

  float ML_11__D_12(char a, float x) {
    float e = 0.0;
    if (a == 'A') e = -4.33280170;
    if (a == 'C') e = -4.03380170;
    if (a == 'G') e = -5.03980170;
    if (a == 'U') e = 1.94819830;
    return x + e + -9.21694027;
  }

  float ML_11__EL(char a, Subsequence x) {
    float e = 0.0;
    if (a == 'A') e = -4.33280170;
    if (a == 'C') e = -4.03380170;
    if (a == 'G') e = -5.03980170;
    if (a == 'U') e = 1.94819830;
    return e + -7.78135971 + size(x) * -0.08926734;
  }

  float D_11__IL_11(float x) {
    return x + -6.17386846;
  }

  float D_11__ML_12(float x) {
    return x + -1.68686846;
  }

  float D_11__D_12(float x) {
    return x + -0.56586846;
  }

  float IL_11__IL_11(char a, float x) {
    float e = 0.0;
    if (a == 'A') e = 0.00000000;
    if (a == 'C') e = 0.00000000;
    if (a == 'G') e = 0.00000000;
    if (a == 'U') e = 0.00000000;
    return x + e + -1.44177497;
  }

  float IL_11__ML_12(char a, float x) {
    float e = 0.0;
    if (a == 'A') e = 0.00000000;
    if (a == 'C') e = 0.00000000;
    if (a == 'G') e = 0.00000000;
    if (a == 'U') e = 0.00000000;
    return x + e + -0.79777497;
  }

  float IL_11__D_12(char a, float x) {
    float e = 0.0;
    if (a == 'A') e = 0.00000000;
    if (a == 'C') e = 0.00000000;
    if (a == 'G') e = 0.00000000;
    if (a == 'U') e = 0.00000000;
    return x + e + -4.14177497;
  }

  float ML_12__E_13(char a, float x) {
    float e = 0.0;
    if (a == 'A') e = -3.79892278;
    if (a == 'C') e = -5.23592278;
    if (a == 'G') e = 1.94807722;
    if (a == 'U') e = -4.53892278;
    return x + e + 0.00000000;
  }

  float D_12__E_13(float x) {
    return x + 0.00000000;
  }

  choice [float] h([float] i) {
    return list(maximum(i));
  }
}

algebra pretty implements Algebra(alphabet = char, answer = alignment) {
  alignment skipl(char a, alignment x) {
	  return x;
  }
  alignment skipr(alignment x, char a) {
	  return x;
  }
  alignment window(Subsequence l, alignment x, Subsequence r) {
    alignment res;
    int start = l.j+1;
    int end = r.i;
    append(res.sequence, start);
    append(res.sequence, ' ');
    append(res.sequence, x.sequence);
    append(res.sequence, ' ');
    append(res.sequence, end);
    append(res.model, x.model);
    return res;
  }

  alignment root__local(alignment x) {
    return x;
  }

  alignment nil(void) {
    alignment res;
    string emptySequence;
    string emptyModel;
    res.sequence = emptySequence; res.model = emptyModel;
    return res;
  }

  alignment root__global(alignment x) {
    alignment res;
    int start = 1;
    int end = 19;
    append(res.model, start);
    append(res.model, ' ');
    append(res.model, x.model);
    append(res.model, ' ');
    append(res.model, end);
    append(res.sequence, x.sequence);
    return res;
  }

  alignment local__MP_2(alignment x) {
    alignment res;
    int start = 2;
    int end = 18;
    append(res.model, start);
    append(res.model, ' ');
    append(res.model, x.model);
    append(res.model, ' ');
    append(res.model, end);
    append(res.sequence, x.sequence);
    return res;
  }

  alignment local__MP_3(alignment x) {
    alignment res;
    int start = 3;
    int end = 17;
    append(res.model, start);
    append(res.model, ' ');
    append(res.model, x.model);
    append(res.model, ' ');
    append(res.model, end);
    append(res.sequence, x.sequence);
    return res;
  }

  alignment local__MP_4(alignment x) {
    alignment res;
    int start = 4;
    int end = 16;
    append(res.model, start);
    append(res.model, ' ');
    append(res.model, x.model);
    append(res.model, ' ');
    append(res.model, end);
    append(res.sequence, x.sequence);
    return res;
  }

  alignment local__MR_5(alignment x) {
    alignment res;
    int start = 15;
    int end = 15;
    append(res.model, start);
    append(res.model, ' ');
    append(res.model, x.model);
    append(res.model, ' ');
    append(res.model, end);
    append(res.sequence, x.sequence);
    return res;
  }

  alignment local__MP_6(alignment x) {
    alignment res;
    int start = 5;
    int end = 14;
    append(res.model, start);
    append(res.model, ' ');
    append(res.model, x.model);
    append(res.model, ' ');
    append(res.model, end);
    append(res.sequence, x.sequence);
    return res;
  }

  alignment local__MP_7(alignment x) {
    alignment res;
    int start = 6;
    int end = 13;
    append(res.model, start);
    append(res.model, ' ');
    append(res.model, x.model);
    append(res.model, ' ');
    append(res.model, end);
    append(res.sequence, x.sequence);
    return res;
  }

  alignment local__MP_8(alignment x) {
    alignment res;
    int start = 7;
    int end = 12;
    append(res.model, start);
    append(res.model, ' ');
    append(res.model, x.model);
    append(res.model, ' ');
    append(res.model, end);
    append(res.sequence, x.sequence);
    return res;
  }

  alignment local__ML_9(alignment x) {
    alignment res;
    int start = 8;
    int end = 8;
    append(res.model, start);
    append(res.model, ' ');
    append(res.model, x.model);
    append(res.model, ' ');
    append(res.model, end);
    append(res.sequence, x.sequence);
    return res;
  }

  alignment local__ML_10(alignment x) {
    alignment res;
    int start = 9;
    int end = 9;
    append(res.model, start);
    append(res.model, ' ');
    append(res.model, x.model);
    append(res.model, ' ');
    append(res.model, end);
    append(res.sequence, x.sequence);
    return res;
  }

  alignment local__ML_11(alignment x) {
    alignment res;
    int start = 10;
    int end = 10;
    append(res.model, start);
    append(res.model, ' ');
    append(res.model, x.model);
    append(res.model, ' ');
    append(res.model, end);
    append(res.sequence, x.sequence);
    return res;
  }

  alignment local__ML_12(alignment x) {
    alignment res;
    int start = 11;
    int end = 11;
    append(res.model, start);
    append(res.model, ' ');
    append(res.model, x.model);
    append(res.model, ' ');
    append(res.model, end);
    append(res.sequence, x.sequence);
    return res;
  }

  alignment MP_1__IL_1(char a, alignment x, char b) {
    alignment res;
    append(res.sequence, a); append(res.model, '<');
    append(res.sequence, x.sequence); append(res.model, x.model);
    append(res.sequence, b); append(res.model, '>');
    return res;
  }

  alignment MP_1__IR_1(char a, alignment x, char b) {
    alignment res;
    append(res.sequence, a); append(res.model, '<');
    append(res.sequence, x.sequence); append(res.model, x.model);
    append(res.sequence, b); append(res.model, '>');
    return res;
  }

  alignment MP_1__MP_2(char a, alignment x, char b) {
    alignment res;
    append(res.sequence, a); append(res.model, '<');
    append(res.sequence, x.sequence); append(res.model, x.model);
    append(res.sequence, b); append(res.model, '>');
    return res;
  }

  alignment MP_1__ML_2(char a, alignment x, char b) {
    alignment res;
    append(res.sequence, a); append(res.model, '<');
    append(res.sequence, x.sequence); append(res.model, x.model);
    append(res.sequence, b); append(res.model, '>');
    return res;
  }

  alignment MP_1__MR_2(char a, alignment x, char b) {
    alignment res;
    append(res.sequence, a); append(res.model, '<');
    append(res.sequence, x.sequence); append(res.model, x.model);
    append(res.sequence, b); append(res.model, '>');
    return res;
  }

  alignment MP_1__D_2(char a, alignment x, char b) {
    alignment res;
    append(res.sequence, a); append(res.model, '<');
    append(res.sequence, x.sequence); append(res.model, x.model);
    append(res.sequence, b); append(res.model, '>');
    return res;
  }

  alignment MP_1__EL(char a, Subsequence x, char b) {
    alignment res;
    append(res.sequence, a); append(res.model, '<');
    int gapLengthSequence = size(x);
    int gapLengthModel = 17;
    float gls = size(x);
    float glm = 17.0;
    int maxLen = log(max(gls, glm))/log(10.0);
    append(res.sequence, "*[", 2);
    append(res.sequence, ' ', 1 + maxLen - log(gls)/log(10.0));
    append(res.sequence, gapLengthSequence);
    append(res.sequence, "]*", 2);
    append(res.model, "*[", 2);
    append(res.model, ' ', 1 + maxLen - log(glm)/log(10.0));
    append(res.model, gapLengthModel);
    append(res.model, "]*", 2);
    append(res.sequence, b); append(res.model, '>');
    return res;
  }

  alignment IL_1__IL_1(char a, alignment x) {
    alignment res;
    append(res.sequence, a); append(res.model, '-');
    append(res.sequence, x.sequence); append(res.model, x.model);
    return res;
  }

  alignment IL_1__IR_1(char a, alignment x) {
    alignment res;
    append(res.sequence, a); append(res.model, '-');
    append(res.sequence, x.sequence); append(res.model, x.model);
    return res;
  }

  alignment IL_1__MP_2(char a, alignment x) {
    alignment res;
    append(res.sequence, a); append(res.model, '-');
    append(res.sequence, x.sequence); append(res.model, x.model);
    return res;
  }

  alignment IL_1__ML_2(char a, alignment x) {
    alignment res;
    append(res.sequence, a); append(res.model, '-');
    append(res.sequence, x.sequence); append(res.model, x.model);
    return res;
  }

  alignment IL_1__MR_2(char a, alignment x) {
    alignment res;
    append(res.sequence, a); append(res.model, '-');
    append(res.sequence, x.sequence); append(res.model, x.model);
    return res;
  }

  alignment IL_1__D_2(char a, alignment x) {
    alignment res;
    append(res.sequence, a); append(res.model, '-');
    append(res.sequence, x.sequence); append(res.model, x.model);
    return res;
  }

  alignment IR_1__IR_1(alignment x, char b) {
    alignment res;
    append(res.sequence, x.sequence); append(res.model, x.model);
    append(res.sequence, b); append(res.model, '-');
    return res;
  }

  alignment IR_1__MP_2(alignment x, char b) {
    alignment res;
    append(res.sequence, x.sequence); append(res.model, x.model);
    append(res.sequence, b); append(res.model, '-');
    return res;
  }

  alignment IR_1__ML_2(alignment x, char b) {
    alignment res;
    append(res.sequence, x.sequence); append(res.model, x.model);
    append(res.sequence, b); append(res.model, '-');
    return res;
  }

  alignment IR_1__MR_2(alignment x, char b) {
    alignment res;
    append(res.sequence, x.sequence); append(res.model, x.model);
    append(res.sequence, b); append(res.model, '-');
    return res;
  }

  alignment IR_1__D_2(alignment x, char b) {
    alignment res;
    append(res.sequence, x.sequence); append(res.model, x.model);
    append(res.sequence, b); append(res.model, '-');
    return res;
  }

  alignment MP_2__IL_2(char a, alignment x, char b) {
    alignment res;
    append(res.sequence, a); append(res.model, '<');
    append(res.sequence, x.sequence); append(res.model, x.model);
    append(res.sequence, b); append(res.model, '>');
    return res;
  }

  alignment MP_2__IR_2(char a, alignment x, char b) {
    alignment res;
    append(res.sequence, a); append(res.model, '<');
    append(res.sequence, x.sequence); append(res.model, x.model);
    append(res.sequence, b); append(res.model, '>');
    return res;
  }

  alignment MP_2__MP_3(char a, alignment x, char b) {
    alignment res;
    append(res.sequence, a); append(res.model, '<');
    append(res.sequence, x.sequence); append(res.model, x.model);
    append(res.sequence, b); append(res.model, '>');
    return res;
  }

  alignment MP_2__ML_3(char a, alignment x, char b) {
    alignment res;
    append(res.sequence, a); append(res.model, '<');
    append(res.sequence, x.sequence); append(res.model, x.model);
    append(res.sequence, b); append(res.model, '>');
    return res;
  }

  alignment MP_2__MR_3(char a, alignment x, char b) {
    alignment res;
    append(res.sequence, a); append(res.model, '<');
    append(res.sequence, x.sequence); append(res.model, x.model);
    append(res.sequence, b); append(res.model, '>');
    return res;
  }

  alignment MP_2__D_3(char a, alignment x, char b) {
    alignment res;
    append(res.sequence, a); append(res.model, '<');
    append(res.sequence, x.sequence); append(res.model, x.model);
    append(res.sequence, b); append(res.model, '>');
    return res;
  }

  alignment MP_2__EL(char a, Subsequence x, char b) {
    alignment res;
    append(res.sequence, a); append(res.model, '<');
    int gapLengthSequence = size(x);
    int gapLengthModel = 15;
    float gls = size(x);
    float glm = 15.0;
    int maxLen = log(max(gls, glm))/log(10.0);
    append(res.sequence, "*[", 2);
    append(res.sequence, ' ', 1 + maxLen - log(gls)/log(10.0));
    append(res.sequence, gapLengthSequence);
    append(res.sequence, "]*", 2);
    append(res.model, "*[", 2);
    append(res.model, ' ', 1 + maxLen - log(glm)/log(10.0));
    append(res.model, gapLengthModel);
    append(res.model, "]*", 2);
    append(res.sequence, b); append(res.model, '>');
    return res;
  }

  alignment ML_2__IL_2(char a, alignment x) {
    alignment res;
    append(res.sequence, a); append(res.model, '<');
    append(res.sequence, x.sequence); append(res.model, x.model);
    append(res.sequence, '-'); append(res.model, '>');
    return res;
  }

  alignment ML_2__IR_2(char a, alignment x) {
    alignment res;
    append(res.sequence, a); append(res.model, '<');
    append(res.sequence, x.sequence); append(res.model, x.model);
    append(res.sequence, '-'); append(res.model, '>');
    return res;
  }

  alignment ML_2__MP_3(char a, alignment x) {
    alignment res;
    append(res.sequence, a); append(res.model, '<');
    append(res.sequence, x.sequence); append(res.model, x.model);
    append(res.sequence, '-'); append(res.model, '>');
    return res;
  }

  alignment ML_2__ML_3(char a, alignment x) {
    alignment res;
    append(res.sequence, a); append(res.model, '<');
    append(res.sequence, x.sequence); append(res.model, x.model);
    append(res.sequence, '-'); append(res.model, '>');
    return res;
  }

  alignment ML_2__MR_3(char a, alignment x) {
    alignment res;
    append(res.sequence, a); append(res.model, '<');
    append(res.sequence, x.sequence); append(res.model, x.model);
    append(res.sequence, '-'); append(res.model, '>');
    return res;
  }

  alignment ML_2__D_3(char a, alignment x) {
    alignment res;
    append(res.sequence, a); append(res.model, '<');
    append(res.sequence, x.sequence); append(res.model, x.model);
    append(res.sequence, '-'); append(res.model, '>');
    return res;
  }

  alignment MR_2__IL_2(alignment x, char b) {
    alignment res;
    append(res.sequence, '-'); append(res.model, '<');
    append(res.sequence, x.sequence); append(res.model, x.model);
    append(res.sequence, b); append(res.model, '>');
    return res;
  }

  alignment MR_2__IR_2(alignment x, char b) {
    alignment res;
    append(res.sequence, '-'); append(res.model, '<');
    append(res.sequence, x.sequence); append(res.model, x.model);
    append(res.sequence, b); append(res.model, '>');
    return res;
  }

  alignment MR_2__MP_3(alignment x, char b) {
    alignment res;
    append(res.sequence, '-'); append(res.model, '<');
    append(res.sequence, x.sequence); append(res.model, x.model);
    append(res.sequence, b); append(res.model, '>');
    return res;
  }

  alignment MR_2__ML_3(alignment x, char b) {
    alignment res;
    append(res.sequence, '-'); append(res.model, '<');
    append(res.sequence, x.sequence); append(res.model, x.model);
    append(res.sequence, b); append(res.model, '>');
    return res;
  }

  alignment MR_2__MR_3(alignment x, char b) {
    alignment res;
    append(res.sequence, '-'); append(res.model, '<');
    append(res.sequence, x.sequence); append(res.model, x.model);
    append(res.sequence, b); append(res.model, '>');
    return res;
  }

  alignment MR_2__D_3(alignment x, char b) {
    alignment res;
    append(res.sequence, '-'); append(res.model, '<');
    append(res.sequence, x.sequence); append(res.model, x.model);
    append(res.sequence, b); append(res.model, '>');
    return res;
  }

  alignment D_2__IL_2(alignment x) {
    alignment res;
    append(res.sequence, '-'); append(res.model, '<');
    append(res.sequence, x.sequence); append(res.model, x.model);
    append(res.sequence, '-'); append(res.model, '>');
    return res;
  }

  alignment D_2__IR_2(alignment x) {
    alignment res;
    append(res.sequence, '-'); append(res.model, '<');
    append(res.sequence, x.sequence); append(res.model, x.model);
    append(res.sequence, '-'); append(res.model, '>');
    return res;
  }

  alignment D_2__MP_3(alignment x) {
    alignment res;
    append(res.sequence, '-'); append(res.model, '<');
    append(res.sequence, x.sequence); append(res.model, x.model);
    append(res.sequence, '-'); append(res.model, '>');
    return res;
  }

  alignment D_2__ML_3(alignment x) {
    alignment res;
    append(res.sequence, '-'); append(res.model, '<');
    append(res.sequence, x.sequence); append(res.model, x.model);
    append(res.sequence, '-'); append(res.model, '>');
    return res;
  }

  alignment D_2__MR_3(alignment x) {
    alignment res;
    append(res.sequence, '-'); append(res.model, '<');
    append(res.sequence, x.sequence); append(res.model, x.model);
    append(res.sequence, '-'); append(res.model, '>');
    return res;
  }

  alignment D_2__D_3(alignment x) {
    alignment res;
    append(res.sequence, '-'); append(res.model, '<');
    append(res.sequence, x.sequence); append(res.model, x.model);
    append(res.sequence, '-'); append(res.model, '>');
    return res;
  }

  alignment IL_2__IL_2(char a, alignment x) {
    alignment res;
    append(res.sequence, a); append(res.model, '-');
    append(res.sequence, x.sequence); append(res.model, x.model);
    return res;
  }

  alignment IL_2__IR_2(char a, alignment x) {
    alignment res;
    append(res.sequence, a); append(res.model, '-');
    append(res.sequence, x.sequence); append(res.model, x.model);
    return res;
  }

  alignment IL_2__MP_3(char a, alignment x) {
    alignment res;
    append(res.sequence, a); append(res.model, '-');
    append(res.sequence, x.sequence); append(res.model, x.model);
    return res;
  }

  alignment IL_2__ML_3(char a, alignment x) {
    alignment res;
    append(res.sequence, a); append(res.model, '-');
    append(res.sequence, x.sequence); append(res.model, x.model);
    return res;
  }

  alignment IL_2__MR_3(char a, alignment x) {
    alignment res;
    append(res.sequence, a); append(res.model, '-');
    append(res.sequence, x.sequence); append(res.model, x.model);
    return res;
  }

  alignment IL_2__D_3(char a, alignment x) {
    alignment res;
    append(res.sequence, a); append(res.model, '-');
    append(res.sequence, x.sequence); append(res.model, x.model);
    return res;
  }

  alignment IR_2__IR_2(alignment x, char b) {
    alignment res;
    append(res.sequence, x.sequence); append(res.model, x.model);
    append(res.sequence, b); append(res.model, '-');
    return res;
  }

  alignment IR_2__MP_3(alignment x, char b) {
    alignment res;
    append(res.sequence, x.sequence); append(res.model, x.model);
    append(res.sequence, b); append(res.model, '-');
    return res;
  }

  alignment IR_2__ML_3(alignment x, char b) {
    alignment res;
    append(res.sequence, x.sequence); append(res.model, x.model);
    append(res.sequence, b); append(res.model, '-');
    return res;
  }

  alignment IR_2__MR_3(alignment x, char b) {
    alignment res;
    append(res.sequence, x.sequence); append(res.model, x.model);
    append(res.sequence, b); append(res.model, '-');
    return res;
  }

  alignment IR_2__D_3(alignment x, char b) {
    alignment res;
    append(res.sequence, x.sequence); append(res.model, x.model);
    append(res.sequence, b); append(res.model, '-');
    return res;
  }

  alignment MP_3__IL_3(char a, alignment x, char b) {
    alignment res;
    append(res.sequence, a); append(res.model, '<');
    append(res.sequence, x.sequence); append(res.model, x.model);
    append(res.sequence, b); append(res.model, '>');
    return res;
  }

  alignment MP_3__IR_3(char a, alignment x, char b) {
    alignment res;
    append(res.sequence, a); append(res.model, '<');
    append(res.sequence, x.sequence); append(res.model, x.model);
    append(res.sequence, b); append(res.model, '>');
    return res;
  }

  alignment MP_3__MP_4(char a, alignment x, char b) {
    alignment res;
    append(res.sequence, a); append(res.model, '<');
    append(res.sequence, x.sequence); append(res.model, x.model);
    append(res.sequence, b); append(res.model, '>');
    return res;
  }

  alignment MP_3__ML_4(char a, alignment x, char b) {
    alignment res;
    append(res.sequence, a); append(res.model, '<');
    append(res.sequence, x.sequence); append(res.model, x.model);
    append(res.sequence, b); append(res.model, '>');
    return res;
  }

  alignment MP_3__MR_4(char a, alignment x, char b) {
    alignment res;
    append(res.sequence, a); append(res.model, '<');
    append(res.sequence, x.sequence); append(res.model, x.model);
    append(res.sequence, b); append(res.model, '>');
    return res;
  }

  alignment MP_3__D_4(char a, alignment x, char b) {
    alignment res;
    append(res.sequence, a); append(res.model, '<');
    append(res.sequence, x.sequence); append(res.model, x.model);
    append(res.sequence, b); append(res.model, '>');
    return res;
  }

  alignment MP_3__EL(char a, Subsequence x, char b) {
    alignment res;
    append(res.sequence, a); append(res.model, '<');
    int gapLengthSequence = size(x);
    int gapLengthModel = 13;
    float gls = size(x);
    float glm = 13.0;
    int maxLen = log(max(gls, glm))/log(10.0);
    append(res.sequence, "*[", 2);
    append(res.sequence, ' ', 1 + maxLen - log(gls)/log(10.0));
    append(res.sequence, gapLengthSequence);
    append(res.sequence, "]*", 2);
    append(res.model, "*[", 2);
    append(res.model, ' ', 1 + maxLen - log(glm)/log(10.0));
    append(res.model, gapLengthModel);
    append(res.model, "]*", 2);
    append(res.sequence, b); append(res.model, '>');
    return res;
  }

  alignment ML_3__IL_3(char a, alignment x) {
    alignment res;
    append(res.sequence, a); append(res.model, '<');
    append(res.sequence, x.sequence); append(res.model, x.model);
    append(res.sequence, '-'); append(res.model, '>');
    return res;
  }

  alignment ML_3__IR_3(char a, alignment x) {
    alignment res;
    append(res.sequence, a); append(res.model, '<');
    append(res.sequence, x.sequence); append(res.model, x.model);
    append(res.sequence, '-'); append(res.model, '>');
    return res;
  }

  alignment ML_3__MP_4(char a, alignment x) {
    alignment res;
    append(res.sequence, a); append(res.model, '<');
    append(res.sequence, x.sequence); append(res.model, x.model);
    append(res.sequence, '-'); append(res.model, '>');
    return res;
  }

  alignment ML_3__ML_4(char a, alignment x) {
    alignment res;
    append(res.sequence, a); append(res.model, '<');
    append(res.sequence, x.sequence); append(res.model, x.model);
    append(res.sequence, '-'); append(res.model, '>');
    return res;
  }

  alignment ML_3__MR_4(char a, alignment x) {
    alignment res;
    append(res.sequence, a); append(res.model, '<');
    append(res.sequence, x.sequence); append(res.model, x.model);
    append(res.sequence, '-'); append(res.model, '>');
    return res;
  }

  alignment ML_3__D_4(char a, alignment x) {
    alignment res;
    append(res.sequence, a); append(res.model, '<');
    append(res.sequence, x.sequence); append(res.model, x.model);
    append(res.sequence, '-'); append(res.model, '>');
    return res;
  }

  alignment MR_3__IL_3(alignment x, char b) {
    alignment res;
    append(res.sequence, '-'); append(res.model, '<');
    append(res.sequence, x.sequence); append(res.model, x.model);
    append(res.sequence, b); append(res.model, '>');
    return res;
  }

  alignment MR_3__IR_3(alignment x, char b) {
    alignment res;
    append(res.sequence, '-'); append(res.model, '<');
    append(res.sequence, x.sequence); append(res.model, x.model);
    append(res.sequence, b); append(res.model, '>');
    return res;
  }

  alignment MR_3__MP_4(alignment x, char b) {
    alignment res;
    append(res.sequence, '-'); append(res.model, '<');
    append(res.sequence, x.sequence); append(res.model, x.model);
    append(res.sequence, b); append(res.model, '>');
    return res;
  }

  alignment MR_3__ML_4(alignment x, char b) {
    alignment res;
    append(res.sequence, '-'); append(res.model, '<');
    append(res.sequence, x.sequence); append(res.model, x.model);
    append(res.sequence, b); append(res.model, '>');
    return res;
  }

  alignment MR_3__MR_4(alignment x, char b) {
    alignment res;
    append(res.sequence, '-'); append(res.model, '<');
    append(res.sequence, x.sequence); append(res.model, x.model);
    append(res.sequence, b); append(res.model, '>');
    return res;
  }

  alignment MR_3__D_4(alignment x, char b) {
    alignment res;
    append(res.sequence, '-'); append(res.model, '<');
    append(res.sequence, x.sequence); append(res.model, x.model);
    append(res.sequence, b); append(res.model, '>');
    return res;
  }

  alignment D_3__IL_3(alignment x) {
    alignment res;
    append(res.sequence, '-'); append(res.model, '<');
    append(res.sequence, x.sequence); append(res.model, x.model);
    append(res.sequence, '-'); append(res.model, '>');
    return res;
  }

  alignment D_3__IR_3(alignment x) {
    alignment res;
    append(res.sequence, '-'); append(res.model, '<');
    append(res.sequence, x.sequence); append(res.model, x.model);
    append(res.sequence, '-'); append(res.model, '>');
    return res;
  }

  alignment D_3__MP_4(alignment x) {
    alignment res;
    append(res.sequence, '-'); append(res.model, '<');
    append(res.sequence, x.sequence); append(res.model, x.model);
    append(res.sequence, '-'); append(res.model, '>');
    return res;
  }

  alignment D_3__ML_4(alignment x) {
    alignment res;
    append(res.sequence, '-'); append(res.model, '<');
    append(res.sequence, x.sequence); append(res.model, x.model);
    append(res.sequence, '-'); append(res.model, '>');
    return res;
  }

  alignment D_3__MR_4(alignment x) {
    alignment res;
    append(res.sequence, '-'); append(res.model, '<');
    append(res.sequence, x.sequence); append(res.model, x.model);
    append(res.sequence, '-'); append(res.model, '>');
    return res;
  }

  alignment D_3__D_4(alignment x) {
    alignment res;
    append(res.sequence, '-'); append(res.model, '<');
    append(res.sequence, x.sequence); append(res.model, x.model);
    append(res.sequence, '-'); append(res.model, '>');
    return res;
  }

  alignment IL_3__IL_3(char a, alignment x) {
    alignment res;
    append(res.sequence, a); append(res.model, '-');
    append(res.sequence, x.sequence); append(res.model, x.model);
    return res;
  }

  alignment IL_3__IR_3(char a, alignment x) {
    alignment res;
    append(res.sequence, a); append(res.model, '-');
    append(res.sequence, x.sequence); append(res.model, x.model);
    return res;
  }

  alignment IL_3__MP_4(char a, alignment x) {
    alignment res;
    append(res.sequence, a); append(res.model, '-');
    append(res.sequence, x.sequence); append(res.model, x.model);
    return res;
  }

  alignment IL_3__ML_4(char a, alignment x) {
    alignment res;
    append(res.sequence, a); append(res.model, '-');
    append(res.sequence, x.sequence); append(res.model, x.model);
    return res;
  }

  alignment IL_3__MR_4(char a, alignment x) {
    alignment res;
    append(res.sequence, a); append(res.model, '-');
    append(res.sequence, x.sequence); append(res.model, x.model);
    return res;
  }

  alignment IL_3__D_4(char a, alignment x) {
    alignment res;
    append(res.sequence, a); append(res.model, '-');
    append(res.sequence, x.sequence); append(res.model, x.model);
    return res;
  }

  alignment IR_3__IR_3(alignment x, char b) {
    alignment res;
    append(res.sequence, x.sequence); append(res.model, x.model);
    append(res.sequence, b); append(res.model, '-');
    return res;
  }

  alignment IR_3__MP_4(alignment x, char b) {
    alignment res;
    append(res.sequence, x.sequence); append(res.model, x.model);
    append(res.sequence, b); append(res.model, '-');
    return res;
  }

  alignment IR_3__ML_4(alignment x, char b) {
    alignment res;
    append(res.sequence, x.sequence); append(res.model, x.model);
    append(res.sequence, b); append(res.model, '-');
    return res;
  }

  alignment IR_3__MR_4(alignment x, char b) {
    alignment res;
    append(res.sequence, x.sequence); append(res.model, x.model);
    append(res.sequence, b); append(res.model, '-');
    return res;
  }

  alignment IR_3__D_4(alignment x, char b) {
    alignment res;
    append(res.sequence, x.sequence); append(res.model, x.model);
    append(res.sequence, b); append(res.model, '-');
    return res;
  }

  alignment MP_4__IL_4(char a, alignment x, char b) {
    alignment res;
    append(res.sequence, a); append(res.model, '<');
    append(res.sequence, x.sequence); append(res.model, x.model);
    append(res.sequence, b); append(res.model, '>');
    return res;
  }

  alignment MP_4__IR_4(char a, alignment x, char b) {
    alignment res;
    append(res.sequence, a); append(res.model, '<');
    append(res.sequence, x.sequence); append(res.model, x.model);
    append(res.sequence, b); append(res.model, '>');
    return res;
  }

  alignment MP_4__MR_5(char a, alignment x, char b) {
    alignment res;
    append(res.sequence, a); append(res.model, '<');
    append(res.sequence, x.sequence); append(res.model, x.model);
    append(res.sequence, b); append(res.model, '>');
    return res;
  }

  alignment MP_4__D_5(char a, alignment x, char b) {
    alignment res;
    append(res.sequence, a); append(res.model, '<');
    append(res.sequence, x.sequence); append(res.model, x.model);
    append(res.sequence, b); append(res.model, '>');
    return res;
  }

  alignment MP_4__EL(char a, Subsequence x, char b) {
    alignment res;
    append(res.sequence, a); append(res.model, '<');
    int gapLengthSequence = size(x);
    int gapLengthModel = 11;
    float gls = size(x);
    float glm = 11.0;
    int maxLen = log(max(gls, glm))/log(10.0);
    append(res.sequence, "*[", 2);
    append(res.sequence, ' ', 1 + maxLen - log(gls)/log(10.0));
    append(res.sequence, gapLengthSequence);
    append(res.sequence, "]*", 2);
    append(res.model, "*[", 2);
    append(res.model, ' ', 1 + maxLen - log(glm)/log(10.0));
    append(res.model, gapLengthModel);
    append(res.model, "]*", 2);
    append(res.sequence, b); append(res.model, '>');
    return res;
  }

  alignment ML_4__IL_4(char a, alignment x) {
    alignment res;
    append(res.sequence, a); append(res.model, '<');
    append(res.sequence, x.sequence); append(res.model, x.model);
    append(res.sequence, '-'); append(res.model, '>');
    return res;
  }

  alignment ML_4__IR_4(char a, alignment x) {
    alignment res;
    append(res.sequence, a); append(res.model, '<');
    append(res.sequence, x.sequence); append(res.model, x.model);
    append(res.sequence, '-'); append(res.model, '>');
    return res;
  }

  alignment ML_4__MR_5(char a, alignment x) {
    alignment res;
    append(res.sequence, a); append(res.model, '<');
    append(res.sequence, x.sequence); append(res.model, x.model);
    append(res.sequence, '-'); append(res.model, '>');
    return res;
  }

  alignment ML_4__D_5(char a, alignment x) {
    alignment res;
    append(res.sequence, a); append(res.model, '<');
    append(res.sequence, x.sequence); append(res.model, x.model);
    append(res.sequence, '-'); append(res.model, '>');
    return res;
  }

  alignment MR_4__IL_4(alignment x, char b) {
    alignment res;
    append(res.sequence, '-'); append(res.model, '<');
    append(res.sequence, x.sequence); append(res.model, x.model);
    append(res.sequence, b); append(res.model, '>');
    return res;
  }

  alignment MR_4__IR_4(alignment x, char b) {
    alignment res;
    append(res.sequence, '-'); append(res.model, '<');
    append(res.sequence, x.sequence); append(res.model, x.model);
    append(res.sequence, b); append(res.model, '>');
    return res;
  }

  alignment MR_4__MR_5(alignment x, char b) {
    alignment res;
    append(res.sequence, '-'); append(res.model, '<');
    append(res.sequence, x.sequence); append(res.model, x.model);
    append(res.sequence, b); append(res.model, '>');
    return res;
  }

  alignment MR_4__D_5(alignment x, char b) {
    alignment res;
    append(res.sequence, '-'); append(res.model, '<');
    append(res.sequence, x.sequence); append(res.model, x.model);
    append(res.sequence, b); append(res.model, '>');
    return res;
  }

  alignment D_4__IL_4(alignment x) {
    alignment res;
    append(res.sequence, '-'); append(res.model, '<');
    append(res.sequence, x.sequence); append(res.model, x.model);
    append(res.sequence, '-'); append(res.model, '>');
    return res;
  }

  alignment D_4__IR_4(alignment x) {
    alignment res;
    append(res.sequence, '-'); append(res.model, '<');
    append(res.sequence, x.sequence); append(res.model, x.model);
    append(res.sequence, '-'); append(res.model, '>');
    return res;
  }

  alignment D_4__MR_5(alignment x) {
    alignment res;
    append(res.sequence, '-'); append(res.model, '<');
    append(res.sequence, x.sequence); append(res.model, x.model);
    append(res.sequence, '-'); append(res.model, '>');
    return res;
  }

  alignment D_4__D_5(alignment x) {
    alignment res;
    append(res.sequence, '-'); append(res.model, '<');
    append(res.sequence, x.sequence); append(res.model, x.model);
    append(res.sequence, '-'); append(res.model, '>');
    return res;
  }

  alignment IL_4__IL_4(char a, alignment x) {
    alignment res;
    append(res.sequence, a); append(res.model, '-');
    append(res.sequence, x.sequence); append(res.model, x.model);
    return res;
  }

  alignment IL_4__IR_4(char a, alignment x) {
    alignment res;
    append(res.sequence, a); append(res.model, '-');
    append(res.sequence, x.sequence); append(res.model, x.model);
    return res;
  }

  alignment IL_4__MR_5(char a, alignment x) {
    alignment res;
    append(res.sequence, a); append(res.model, '-');
    append(res.sequence, x.sequence); append(res.model, x.model);
    return res;
  }

  alignment IL_4__D_5(char a, alignment x) {
    alignment res;
    append(res.sequence, a); append(res.model, '-');
    append(res.sequence, x.sequence); append(res.model, x.model);
    return res;
  }

  alignment IR_4__IR_4(alignment x, char b) {
    alignment res;
    append(res.sequence, x.sequence); append(res.model, x.model);
    append(res.sequence, b); append(res.model, '-');
    return res;
  }

  alignment IR_4__MR_5(alignment x, char b) {
    alignment res;
    append(res.sequence, x.sequence); append(res.model, x.model);
    append(res.sequence, b); append(res.model, '-');
    return res;
  }

  alignment IR_4__D_5(alignment x, char b) {
    alignment res;
    append(res.sequence, x.sequence); append(res.model, x.model);
    append(res.sequence, b); append(res.model, '-');
    return res;
  }

  alignment MR_5__IR_5(alignment x, char b) {
    alignment res;
    append(res.sequence, x.sequence); append(res.model, x.model);
    append(res.sequence, b); append(res.model, '.');
    return res;
  }

  alignment MR_5__MP_6(alignment x, char b) {
    alignment res;
    append(res.sequence, x.sequence); append(res.model, x.model);
    append(res.sequence, b); append(res.model, '.');
    return res;
  }

  alignment MR_5__ML_6(alignment x, char b) {
    alignment res;
    append(res.sequence, x.sequence); append(res.model, x.model);
    append(res.sequence, b); append(res.model, '.');
    return res;
  }

  alignment MR_5__MR_6(alignment x, char b) {
    alignment res;
    append(res.sequence, x.sequence); append(res.model, x.model);
    append(res.sequence, b); append(res.model, '.');
    return res;
  }

  alignment MR_5__D_6(alignment x, char b) {
    alignment res;
    append(res.sequence, x.sequence); append(res.model, x.model);
    append(res.sequence, b); append(res.model, '.');
    return res;
  }

  alignment MR_5__EL(Subsequence x, char b) {
    alignment res;
    int gapLengthSequence = size(x);
    int gapLengthModel = 10;
    float gls = size(x);
    float glm = 10.0;
    int maxLen = log(max(gls, glm))/log(10.0);
    append(res.sequence, "*[", 2);
    append(res.sequence, ' ', 1 + maxLen - log(gls)/log(10.0));
    append(res.sequence, gapLengthSequence);
    append(res.sequence, "]*", 2);
    append(res.model, "*[", 2);
    append(res.model, ' ', 1 + maxLen - log(glm)/log(10.0));
    append(res.model, gapLengthModel);
    append(res.model, "]*", 2);
    append(res.sequence, b); append(res.model, '.');
    return res;
  }

  alignment D_5__IR_5(alignment x) {
    alignment res;
    append(res.sequence, x.sequence); append(res.model, x.model);
    append(res.sequence, '-'); append(res.model, '.');
    return res;
  }

  alignment D_5__MP_6(alignment x) {
    alignment res;
    append(res.sequence, x.sequence); append(res.model, x.model);
    append(res.sequence, '-'); append(res.model, '.');
    return res;
  }

  alignment D_5__ML_6(alignment x) {
    alignment res;
    append(res.sequence, x.sequence); append(res.model, x.model);
    append(res.sequence, '-'); append(res.model, '.');
    return res;
  }

  alignment D_5__MR_6(alignment x) {
    alignment res;
    append(res.sequence, x.sequence); append(res.model, x.model);
    append(res.sequence, '-'); append(res.model, '.');
    return res;
  }

  alignment D_5__D_6(alignment x) {
    alignment res;
    append(res.sequence, x.sequence); append(res.model, x.model);
    append(res.sequence, '-'); append(res.model, '.');
    return res;
  }

  alignment IR_5__IR_5(alignment x, char b) {
    alignment res;
    append(res.sequence, x.sequence); append(res.model, x.model);
    append(res.sequence, b); append(res.model, '-');
    return res;
  }

  alignment IR_5__MP_6(alignment x, char b) {
    alignment res;
    append(res.sequence, x.sequence); append(res.model, x.model);
    append(res.sequence, b); append(res.model, '-');
    return res;
  }

  alignment IR_5__ML_6(alignment x, char b) {
    alignment res;
    append(res.sequence, x.sequence); append(res.model, x.model);
    append(res.sequence, b); append(res.model, '-');
    return res;
  }

  alignment IR_5__MR_6(alignment x, char b) {
    alignment res;
    append(res.sequence, x.sequence); append(res.model, x.model);
    append(res.sequence, b); append(res.model, '-');
    return res;
  }

  alignment IR_5__D_6(alignment x, char b) {
    alignment res;
    append(res.sequence, x.sequence); append(res.model, x.model);
    append(res.sequence, b); append(res.model, '-');
    return res;
  }

  alignment MP_6__IL_6(char a, alignment x, char b) {
    alignment res;
    append(res.sequence, a); append(res.model, '<');
    append(res.sequence, x.sequence); append(res.model, x.model);
    append(res.sequence, b); append(res.model, '>');
    return res;
  }

  alignment MP_6__IR_6(char a, alignment x, char b) {
    alignment res;
    append(res.sequence, a); append(res.model, '<');
    append(res.sequence, x.sequence); append(res.model, x.model);
    append(res.sequence, b); append(res.model, '>');
    return res;
  }

  alignment MP_6__MP_7(char a, alignment x, char b) {
    alignment res;
    append(res.sequence, a); append(res.model, '<');
    append(res.sequence, x.sequence); append(res.model, x.model);
    append(res.sequence, b); append(res.model, '>');
    return res;
  }

  alignment MP_6__ML_7(char a, alignment x, char b) {
    alignment res;
    append(res.sequence, a); append(res.model, '<');
    append(res.sequence, x.sequence); append(res.model, x.model);
    append(res.sequence, b); append(res.model, '>');
    return res;
  }

  alignment MP_6__MR_7(char a, alignment x, char b) {
    alignment res;
    append(res.sequence, a); append(res.model, '<');
    append(res.sequence, x.sequence); append(res.model, x.model);
    append(res.sequence, b); append(res.model, '>');
    return res;
  }

  alignment MP_6__D_7(char a, alignment x, char b) {
    alignment res;
    append(res.sequence, a); append(res.model, '<');
    append(res.sequence, x.sequence); append(res.model, x.model);
    append(res.sequence, b); append(res.model, '>');
    return res;
  }

  alignment MP_6__EL(char a, Subsequence x, char b) {
    alignment res;
    append(res.sequence, a); append(res.model, '<');
    int gapLengthSequence = size(x);
    int gapLengthModel = 8;
    float gls = size(x);
    float glm = 8.0;
    int maxLen = log(max(gls, glm))/log(10.0);
    append(res.sequence, "*[", 2);
    append(res.sequence, ' ', 1 + maxLen - log(gls)/log(10.0));
    append(res.sequence, gapLengthSequence);
    append(res.sequence, "]*", 2);
    append(res.model, "*[", 2);
    append(res.model, ' ', 1 + maxLen - log(glm)/log(10.0));
    append(res.model, gapLengthModel);
    append(res.model, "]*", 2);
    append(res.sequence, b); append(res.model, '>');
    return res;
  }

  alignment ML_6__IL_6(char a, alignment x) {
    alignment res;
    append(res.sequence, a); append(res.model, '<');
    append(res.sequence, x.sequence); append(res.model, x.model);
    append(res.sequence, '-'); append(res.model, '>');
    return res;
  }

  alignment ML_6__IR_6(char a, alignment x) {
    alignment res;
    append(res.sequence, a); append(res.model, '<');
    append(res.sequence, x.sequence); append(res.model, x.model);
    append(res.sequence, '-'); append(res.model, '>');
    return res;
  }

  alignment ML_6__MP_7(char a, alignment x) {
    alignment res;
    append(res.sequence, a); append(res.model, '<');
    append(res.sequence, x.sequence); append(res.model, x.model);
    append(res.sequence, '-'); append(res.model, '>');
    return res;
  }

  alignment ML_6__ML_7(char a, alignment x) {
    alignment res;
    append(res.sequence, a); append(res.model, '<');
    append(res.sequence, x.sequence); append(res.model, x.model);
    append(res.sequence, '-'); append(res.model, '>');
    return res;
  }

  alignment ML_6__MR_7(char a, alignment x) {
    alignment res;
    append(res.sequence, a); append(res.model, '<');
    append(res.sequence, x.sequence); append(res.model, x.model);
    append(res.sequence, '-'); append(res.model, '>');
    return res;
  }

  alignment ML_6__D_7(char a, alignment x) {
    alignment res;
    append(res.sequence, a); append(res.model, '<');
    append(res.sequence, x.sequence); append(res.model, x.model);
    append(res.sequence, '-'); append(res.model, '>');
    return res;
  }

  alignment MR_6__IL_6(alignment x, char b) {
    alignment res;
    append(res.sequence, '-'); append(res.model, '<');
    append(res.sequence, x.sequence); append(res.model, x.model);
    append(res.sequence, b); append(res.model, '>');
    return res;
  }

  alignment MR_6__IR_6(alignment x, char b) {
    alignment res;
    append(res.sequence, '-'); append(res.model, '<');
    append(res.sequence, x.sequence); append(res.model, x.model);
    append(res.sequence, b); append(res.model, '>');
    return res;
  }

  alignment MR_6__MP_7(alignment x, char b) {
    alignment res;
    append(res.sequence, '-'); append(res.model, '<');
    append(res.sequence, x.sequence); append(res.model, x.model);
    append(res.sequence, b); append(res.model, '>');
    return res;
  }

  alignment MR_6__ML_7(alignment x, char b) {
    alignment res;
    append(res.sequence, '-'); append(res.model, '<');
    append(res.sequence, x.sequence); append(res.model, x.model);
    append(res.sequence, b); append(res.model, '>');
    return res;
  }

  alignment MR_6__MR_7(alignment x, char b) {
    alignment res;
    append(res.sequence, '-'); append(res.model, '<');
    append(res.sequence, x.sequence); append(res.model, x.model);
    append(res.sequence, b); append(res.model, '>');
    return res;
  }

  alignment MR_6__D_7(alignment x, char b) {
    alignment res;
    append(res.sequence, '-'); append(res.model, '<');
    append(res.sequence, x.sequence); append(res.model, x.model);
    append(res.sequence, b); append(res.model, '>');
    return res;
  }

  alignment D_6__IL_6(alignment x) {
    alignment res;
    append(res.sequence, '-'); append(res.model, '<');
    append(res.sequence, x.sequence); append(res.model, x.model);
    append(res.sequence, '-'); append(res.model, '>');
    return res;
  }

  alignment D_6__IR_6(alignment x) {
    alignment res;
    append(res.sequence, '-'); append(res.model, '<');
    append(res.sequence, x.sequence); append(res.model, x.model);
    append(res.sequence, '-'); append(res.model, '>');
    return res;
  }

  alignment D_6__MP_7(alignment x) {
    alignment res;
    append(res.sequence, '-'); append(res.model, '<');
    append(res.sequence, x.sequence); append(res.model, x.model);
    append(res.sequence, '-'); append(res.model, '>');
    return res;
  }

  alignment D_6__ML_7(alignment x) {
    alignment res;
    append(res.sequence, '-'); append(res.model, '<');
    append(res.sequence, x.sequence); append(res.model, x.model);
    append(res.sequence, '-'); append(res.model, '>');
    return res;
  }

  alignment D_6__MR_7(alignment x) {
    alignment res;
    append(res.sequence, '-'); append(res.model, '<');
    append(res.sequence, x.sequence); append(res.model, x.model);
    append(res.sequence, '-'); append(res.model, '>');
    return res;
  }

  alignment D_6__D_7(alignment x) {
    alignment res;
    append(res.sequence, '-'); append(res.model, '<');
    append(res.sequence, x.sequence); append(res.model, x.model);
    append(res.sequence, '-'); append(res.model, '>');
    return res;
  }

  alignment IL_6__IL_6(char a, alignment x) {
    alignment res;
    append(res.sequence, a); append(res.model, '-');
    append(res.sequence, x.sequence); append(res.model, x.model);
    return res;
  }

  alignment IL_6__IR_6(char a, alignment x) {
    alignment res;
    append(res.sequence, a); append(res.model, '-');
    append(res.sequence, x.sequence); append(res.model, x.model);
    return res;
  }

  alignment IL_6__MP_7(char a, alignment x) {
    alignment res;
    append(res.sequence, a); append(res.model, '-');
    append(res.sequence, x.sequence); append(res.model, x.model);
    return res;
  }

  alignment IL_6__ML_7(char a, alignment x) {
    alignment res;
    append(res.sequence, a); append(res.model, '-');
    append(res.sequence, x.sequence); append(res.model, x.model);
    return res;
  }

  alignment IL_6__MR_7(char a, alignment x) {
    alignment res;
    append(res.sequence, a); append(res.model, '-');
    append(res.sequence, x.sequence); append(res.model, x.model);
    return res;
  }

  alignment IL_6__D_7(char a, alignment x) {
    alignment res;
    append(res.sequence, a); append(res.model, '-');
    append(res.sequence, x.sequence); append(res.model, x.model);
    return res;
  }

  alignment IR_6__IR_6(alignment x, char b) {
    alignment res;
    append(res.sequence, x.sequence); append(res.model, x.model);
    append(res.sequence, b); append(res.model, '-');
    return res;
  }

  alignment IR_6__MP_7(alignment x, char b) {
    alignment res;
    append(res.sequence, x.sequence); append(res.model, x.model);
    append(res.sequence, b); append(res.model, '-');
    return res;
  }

  alignment IR_6__ML_7(alignment x, char b) {
    alignment res;
    append(res.sequence, x.sequence); append(res.model, x.model);
    append(res.sequence, b); append(res.model, '-');
    return res;
  }

  alignment IR_6__MR_7(alignment x, char b) {
    alignment res;
    append(res.sequence, x.sequence); append(res.model, x.model);
    append(res.sequence, b); append(res.model, '-');
    return res;
  }

  alignment IR_6__D_7(alignment x, char b) {
    alignment res;
    append(res.sequence, x.sequence); append(res.model, x.model);
    append(res.sequence, b); append(res.model, '-');
    return res;
  }

  alignment MP_7__IL_7(char a, alignment x, char b) {
    alignment res;
    append(res.sequence, a); append(res.model, '<');
    append(res.sequence, x.sequence); append(res.model, x.model);
    append(res.sequence, b); append(res.model, '>');
    return res;
  }

  alignment MP_7__IR_7(char a, alignment x, char b) {
    alignment res;
    append(res.sequence, a); append(res.model, '<');
    append(res.sequence, x.sequence); append(res.model, x.model);
    append(res.sequence, b); append(res.model, '>');
    return res;
  }

  alignment MP_7__MP_8(char a, alignment x, char b) {
    alignment res;
    append(res.sequence, a); append(res.model, '<');
    append(res.sequence, x.sequence); append(res.model, x.model);
    append(res.sequence, b); append(res.model, '>');
    return res;
  }

  alignment MP_7__ML_8(char a, alignment x, char b) {
    alignment res;
    append(res.sequence, a); append(res.model, '<');
    append(res.sequence, x.sequence); append(res.model, x.model);
    append(res.sequence, b); append(res.model, '>');
    return res;
  }

  alignment MP_7__MR_8(char a, alignment x, char b) {
    alignment res;
    append(res.sequence, a); append(res.model, '<');
    append(res.sequence, x.sequence); append(res.model, x.model);
    append(res.sequence, b); append(res.model, '>');
    return res;
  }

  alignment MP_7__D_8(char a, alignment x, char b) {
    alignment res;
    append(res.sequence, a); append(res.model, '<');
    append(res.sequence, x.sequence); append(res.model, x.model);
    append(res.sequence, b); append(res.model, '>');
    return res;
  }

  alignment MP_7__EL(char a, Subsequence x, char b) {
    alignment res;
    append(res.sequence, a); append(res.model, '<');
    int gapLengthSequence = size(x);
    int gapLengthModel = 6;
    float gls = size(x);
    float glm = 6.0;
    int maxLen = log(max(gls, glm))/log(10.0);
    append(res.sequence, "*[", 2);
    append(res.sequence, ' ', 1 + maxLen - log(gls)/log(10.0));
    append(res.sequence, gapLengthSequence);
    append(res.sequence, "]*", 2);
    append(res.model, "*[", 2);
    append(res.model, ' ', 1 + maxLen - log(glm)/log(10.0));
    append(res.model, gapLengthModel);
    append(res.model, "]*", 2);
    append(res.sequence, b); append(res.model, '>');
    return res;
  }

  alignment ML_7__IL_7(char a, alignment x) {
    alignment res;
    append(res.sequence, a); append(res.model, '<');
    append(res.sequence, x.sequence); append(res.model, x.model);
    append(res.sequence, '-'); append(res.model, '>');
    return res;
  }

  alignment ML_7__IR_7(char a, alignment x) {
    alignment res;
    append(res.sequence, a); append(res.model, '<');
    append(res.sequence, x.sequence); append(res.model, x.model);
    append(res.sequence, '-'); append(res.model, '>');
    return res;
  }

  alignment ML_7__MP_8(char a, alignment x) {
    alignment res;
    append(res.sequence, a); append(res.model, '<');
    append(res.sequence, x.sequence); append(res.model, x.model);
    append(res.sequence, '-'); append(res.model, '>');
    return res;
  }

  alignment ML_7__ML_8(char a, alignment x) {
    alignment res;
    append(res.sequence, a); append(res.model, '<');
    append(res.sequence, x.sequence); append(res.model, x.model);
    append(res.sequence, '-'); append(res.model, '>');
    return res;
  }

  alignment ML_7__MR_8(char a, alignment x) {
    alignment res;
    append(res.sequence, a); append(res.model, '<');
    append(res.sequence, x.sequence); append(res.model, x.model);
    append(res.sequence, '-'); append(res.model, '>');
    return res;
  }

  alignment ML_7__D_8(char a, alignment x) {
    alignment res;
    append(res.sequence, a); append(res.model, '<');
    append(res.sequence, x.sequence); append(res.model, x.model);
    append(res.sequence, '-'); append(res.model, '>');
    return res;
  }

  alignment MR_7__IL_7(alignment x, char b) {
    alignment res;
    append(res.sequence, '-'); append(res.model, '<');
    append(res.sequence, x.sequence); append(res.model, x.model);
    append(res.sequence, b); append(res.model, '>');
    return res;
  }

  alignment MR_7__IR_7(alignment x, char b) {
    alignment res;
    append(res.sequence, '-'); append(res.model, '<');
    append(res.sequence, x.sequence); append(res.model, x.model);
    append(res.sequence, b); append(res.model, '>');
    return res;
  }

  alignment MR_7__MP_8(alignment x, char b) {
    alignment res;
    append(res.sequence, '-'); append(res.model, '<');
    append(res.sequence, x.sequence); append(res.model, x.model);
    append(res.sequence, b); append(res.model, '>');
    return res;
  }

  alignment MR_7__ML_8(alignment x, char b) {
    alignment res;
    append(res.sequence, '-'); append(res.model, '<');
    append(res.sequence, x.sequence); append(res.model, x.model);
    append(res.sequence, b); append(res.model, '>');
    return res;
  }

  alignment MR_7__MR_8(alignment x, char b) {
    alignment res;
    append(res.sequence, '-'); append(res.model, '<');
    append(res.sequence, x.sequence); append(res.model, x.model);
    append(res.sequence, b); append(res.model, '>');
    return res;
  }

  alignment MR_7__D_8(alignment x, char b) {
    alignment res;
    append(res.sequence, '-'); append(res.model, '<');
    append(res.sequence, x.sequence); append(res.model, x.model);
    append(res.sequence, b); append(res.model, '>');
    return res;
  }

  alignment D_7__IL_7(alignment x) {
    alignment res;
    append(res.sequence, '-'); append(res.model, '<');
    append(res.sequence, x.sequence); append(res.model, x.model);
    append(res.sequence, '-'); append(res.model, '>');
    return res;
  }

  alignment D_7__IR_7(alignment x) {
    alignment res;
    append(res.sequence, '-'); append(res.model, '<');
    append(res.sequence, x.sequence); append(res.model, x.model);
    append(res.sequence, '-'); append(res.model, '>');
    return res;
  }

  alignment D_7__MP_8(alignment x) {
    alignment res;
    append(res.sequence, '-'); append(res.model, '<');
    append(res.sequence, x.sequence); append(res.model, x.model);
    append(res.sequence, '-'); append(res.model, '>');
    return res;
  }

  alignment D_7__ML_8(alignment x) {
    alignment res;
    append(res.sequence, '-'); append(res.model, '<');
    append(res.sequence, x.sequence); append(res.model, x.model);
    append(res.sequence, '-'); append(res.model, '>');
    return res;
  }

  alignment D_7__MR_8(alignment x) {
    alignment res;
    append(res.sequence, '-'); append(res.model, '<');
    append(res.sequence, x.sequence); append(res.model, x.model);
    append(res.sequence, '-'); append(res.model, '>');
    return res;
  }

  alignment D_7__D_8(alignment x) {
    alignment res;
    append(res.sequence, '-'); append(res.model, '<');
    append(res.sequence, x.sequence); append(res.model, x.model);
    append(res.sequence, '-'); append(res.model, '>');
    return res;
  }

  alignment IL_7__IL_7(char a, alignment x) {
    alignment res;
    append(res.sequence, a); append(res.model, '-');
    append(res.sequence, x.sequence); append(res.model, x.model);
    return res;
  }

  alignment IL_7__IR_7(char a, alignment x) {
    alignment res;
    append(res.sequence, a); append(res.model, '-');
    append(res.sequence, x.sequence); append(res.model, x.model);
    return res;
  }

  alignment IL_7__MP_8(char a, alignment x) {
    alignment res;
    append(res.sequence, a); append(res.model, '-');
    append(res.sequence, x.sequence); append(res.model, x.model);
    return res;
  }

  alignment IL_7__ML_8(char a, alignment x) {
    alignment res;
    append(res.sequence, a); append(res.model, '-');
    append(res.sequence, x.sequence); append(res.model, x.model);
    return res;
  }

  alignment IL_7__MR_8(char a, alignment x) {
    alignment res;
    append(res.sequence, a); append(res.model, '-');
    append(res.sequence, x.sequence); append(res.model, x.model);
    return res;
  }

  alignment IL_7__D_8(char a, alignment x) {
    alignment res;
    append(res.sequence, a); append(res.model, '-');
    append(res.sequence, x.sequence); append(res.model, x.model);
    return res;
  }

  alignment IR_7__IR_7(alignment x, char b) {
    alignment res;
    append(res.sequence, x.sequence); append(res.model, x.model);
    append(res.sequence, b); append(res.model, '-');
    return res;
  }

  alignment IR_7__MP_8(alignment x, char b) {
    alignment res;
    append(res.sequence, x.sequence); append(res.model, x.model);
    append(res.sequence, b); append(res.model, '-');
    return res;
  }

  alignment IR_7__ML_8(alignment x, char b) {
    alignment res;
    append(res.sequence, x.sequence); append(res.model, x.model);
    append(res.sequence, b); append(res.model, '-');
    return res;
  }

  alignment IR_7__MR_8(alignment x, char b) {
    alignment res;
    append(res.sequence, x.sequence); append(res.model, x.model);
    append(res.sequence, b); append(res.model, '-');
    return res;
  }

  alignment IR_7__D_8(alignment x, char b) {
    alignment res;
    append(res.sequence, x.sequence); append(res.model, x.model);
    append(res.sequence, b); append(res.model, '-');
    return res;
  }

  alignment MP_8__IL_8(char a, alignment x, char b) {
    alignment res;
    append(res.sequence, a); append(res.model, '<');
    append(res.sequence, x.sequence); append(res.model, x.model);
    append(res.sequence, b); append(res.model, '>');
    return res;
  }

  alignment MP_8__IR_8(char a, alignment x, char b) {
    alignment res;
    append(res.sequence, a); append(res.model, '<');
    append(res.sequence, x.sequence); append(res.model, x.model);
    append(res.sequence, b); append(res.model, '>');
    return res;
  }

  alignment MP_8__ML_9(char a, alignment x, char b) {
    alignment res;
    append(res.sequence, a); append(res.model, '<');
    append(res.sequence, x.sequence); append(res.model, x.model);
    append(res.sequence, b); append(res.model, '>');
    return res;
  }

  alignment MP_8__D_9(char a, alignment x, char b) {
    alignment res;
    append(res.sequence, a); append(res.model, '<');
    append(res.sequence, x.sequence); append(res.model, x.model);
    append(res.sequence, b); append(res.model, '>');
    return res;
  }

  alignment MP_8__EL(char a, Subsequence x, char b) {
    alignment res;
    append(res.sequence, a); append(res.model, '<');
    int gapLengthSequence = size(x);
    int gapLengthModel = 4;
    float gls = size(x);
    float glm = 4.0;
    int maxLen = log(max(gls, glm))/log(10.0);
    append(res.sequence, "*[", 2);
    append(res.sequence, ' ', 1 + maxLen - log(gls)/log(10.0));
    append(res.sequence, gapLengthSequence);
    append(res.sequence, "]*", 2);
    append(res.model, "*[", 2);
    append(res.model, ' ', 1 + maxLen - log(glm)/log(10.0));
    append(res.model, gapLengthModel);
    append(res.model, "]*", 2);
    append(res.sequence, b); append(res.model, '>');
    return res;
  }

  alignment ML_8__IL_8(char a, alignment x) {
    alignment res;
    append(res.sequence, a); append(res.model, '<');
    append(res.sequence, x.sequence); append(res.model, x.model);
    append(res.sequence, '-'); append(res.model, '>');
    return res;
  }

  alignment ML_8__IR_8(char a, alignment x) {
    alignment res;
    append(res.sequence, a); append(res.model, '<');
    append(res.sequence, x.sequence); append(res.model, x.model);
    append(res.sequence, '-'); append(res.model, '>');
    return res;
  }

  alignment ML_8__ML_9(char a, alignment x) {
    alignment res;
    append(res.sequence, a); append(res.model, '<');
    append(res.sequence, x.sequence); append(res.model, x.model);
    append(res.sequence, '-'); append(res.model, '>');
    return res;
  }

  alignment ML_8__D_9(char a, alignment x) {
    alignment res;
    append(res.sequence, a); append(res.model, '<');
    append(res.sequence, x.sequence); append(res.model, x.model);
    append(res.sequence, '-'); append(res.model, '>');
    return res;
  }

  alignment MR_8__IL_8(alignment x, char b) {
    alignment res;
    append(res.sequence, '-'); append(res.model, '<');
    append(res.sequence, x.sequence); append(res.model, x.model);
    append(res.sequence, b); append(res.model, '>');
    return res;
  }

  alignment MR_8__IR_8(alignment x, char b) {
    alignment res;
    append(res.sequence, '-'); append(res.model, '<');
    append(res.sequence, x.sequence); append(res.model, x.model);
    append(res.sequence, b); append(res.model, '>');
    return res;
  }

  alignment MR_8__ML_9(alignment x, char b) {
    alignment res;
    append(res.sequence, '-'); append(res.model, '<');
    append(res.sequence, x.sequence); append(res.model, x.model);
    append(res.sequence, b); append(res.model, '>');
    return res;
  }

  alignment MR_8__D_9(alignment x, char b) {
    alignment res;
    append(res.sequence, '-'); append(res.model, '<');
    append(res.sequence, x.sequence); append(res.model, x.model);
    append(res.sequence, b); append(res.model, '>');
    return res;
  }

  alignment D_8__IL_8(alignment x) {
    alignment res;
    append(res.sequence, '-'); append(res.model, '<');
    append(res.sequence, x.sequence); append(res.model, x.model);
    append(res.sequence, '-'); append(res.model, '>');
    return res;
  }

  alignment D_8__IR_8(alignment x) {
    alignment res;
    append(res.sequence, '-'); append(res.model, '<');
    append(res.sequence, x.sequence); append(res.model, x.model);
    append(res.sequence, '-'); append(res.model, '>');
    return res;
  }

  alignment D_8__ML_9(alignment x) {
    alignment res;
    append(res.sequence, '-'); append(res.model, '<');
    append(res.sequence, x.sequence); append(res.model, x.model);
    append(res.sequence, '-'); append(res.model, '>');
    return res;
  }

  alignment D_8__D_9(alignment x) {
    alignment res;
    append(res.sequence, '-'); append(res.model, '<');
    append(res.sequence, x.sequence); append(res.model, x.model);
    append(res.sequence, '-'); append(res.model, '>');
    return res;
  }

  alignment IL_8__IL_8(char a, alignment x) {
    alignment res;
    append(res.sequence, a); append(res.model, '-');
    append(res.sequence, x.sequence); append(res.model, x.model);
    return res;
  }

  alignment IL_8__IR_8(char a, alignment x) {
    alignment res;
    append(res.sequence, a); append(res.model, '-');
    append(res.sequence, x.sequence); append(res.model, x.model);
    return res;
  }

  alignment IL_8__ML_9(char a, alignment x) {
    alignment res;
    append(res.sequence, a); append(res.model, '-');
    append(res.sequence, x.sequence); append(res.model, x.model);
    return res;
  }

  alignment IL_8__D_9(char a, alignment x) {
    alignment res;
    append(res.sequence, a); append(res.model, '-');
    append(res.sequence, x.sequence); append(res.model, x.model);
    return res;
  }

  alignment IR_8__IR_8(alignment x, char b) {
    alignment res;
    append(res.sequence, x.sequence); append(res.model, x.model);
    append(res.sequence, b); append(res.model, '-');
    return res;
  }

  alignment IR_8__ML_9(alignment x, char b) {
    alignment res;
    append(res.sequence, x.sequence); append(res.model, x.model);
    append(res.sequence, b); append(res.model, '-');
    return res;
  }

  alignment IR_8__D_9(alignment x, char b) {
    alignment res;
    append(res.sequence, x.sequence); append(res.model, x.model);
    append(res.sequence, b); append(res.model, '-');
    return res;
  }

  alignment ML_9__IL_9(char a, alignment x) {
    alignment res;
    append(res.sequence, a); append(res.model, '.');
    append(res.sequence, x.sequence); append(res.model, x.model);
    return res;
  }

  alignment ML_9__ML_10(char a, alignment x) {
    alignment res;
    append(res.sequence, a); append(res.model, '.');
    append(res.sequence, x.sequence); append(res.model, x.model);
    return res;
  }

  alignment ML_9__D_10(char a, alignment x) {
    alignment res;
    append(res.sequence, a); append(res.model, '.');
    append(res.sequence, x.sequence); append(res.model, x.model);
    return res;
  }

  alignment ML_9__EL(char a, Subsequence x) {
    alignment res;
    append(res.sequence, a); append(res.model, '.');
    int gapLengthSequence = size(x);
    int gapLengthModel = 3;
    float gls = size(x);
    float glm = 3.0;
    int maxLen = log(max(gls, glm))/log(10.0);
    append(res.sequence, "*[", 2);
    append(res.sequence, ' ', 1 + maxLen - log(gls)/log(10.0));
    append(res.sequence, gapLengthSequence);
    append(res.sequence, "]*", 2);
    append(res.model, "*[", 2);
    append(res.model, ' ', 1 + maxLen - log(glm)/log(10.0));
    append(res.model, gapLengthModel);
    append(res.model, "]*", 2);
    return res;
  }

  alignment D_9__IL_9(alignment x) {
    alignment res;
    append(res.sequence, '-'); append(res.model, '.');
    append(res.sequence, x.sequence); append(res.model, x.model);
    return res;
  }

  alignment D_9__ML_10(alignment x) {
    alignment res;
    append(res.sequence, '-'); append(res.model, '.');
    append(res.sequence, x.sequence); append(res.model, x.model);
    return res;
  }

  alignment D_9__D_10(alignment x) {
    alignment res;
    append(res.sequence, '-'); append(res.model, '.');
    append(res.sequence, x.sequence); append(res.model, x.model);
    return res;
  }

  alignment IL_9__IL_9(char a, alignment x) {
    alignment res;
    append(res.sequence, a); append(res.model, '-');
    append(res.sequence, x.sequence); append(res.model, x.model);
    return res;
  }

  alignment IL_9__ML_10(char a, alignment x) {
    alignment res;
    append(res.sequence, a); append(res.model, '-');
    append(res.sequence, x.sequence); append(res.model, x.model);
    return res;
  }

  alignment IL_9__D_10(char a, alignment x) {
    alignment res;
    append(res.sequence, a); append(res.model, '-');
    append(res.sequence, x.sequence); append(res.model, x.model);
    return res;
  }

  alignment ML_10__IL_10(char a, alignment x) {
    alignment res;
    append(res.sequence, a); append(res.model, '.');
    append(res.sequence, x.sequence); append(res.model, x.model);
    return res;
  }

  alignment ML_10__ML_11(char a, alignment x) {
    alignment res;
    append(res.sequence, a); append(res.model, '.');
    append(res.sequence, x.sequence); append(res.model, x.model);
    return res;
  }

  alignment ML_10__D_11(char a, alignment x) {
    alignment res;
    append(res.sequence, a); append(res.model, '.');
    append(res.sequence, x.sequence); append(res.model, x.model);
    return res;
  }

  alignment ML_10__EL(char a, Subsequence x) {
    alignment res;
    append(res.sequence, a); append(res.model, '.');
    int gapLengthSequence = size(x);
    int gapLengthModel = 2;
    float gls = size(x);
    float glm = 2.0;
    int maxLen = log(max(gls, glm))/log(10.0);
    append(res.sequence, "*[", 2);
    append(res.sequence, ' ', 1 + maxLen - log(gls)/log(10.0));
    append(res.sequence, gapLengthSequence);
    append(res.sequence, "]*", 2);
    append(res.model, "*[", 2);
    append(res.model, ' ', 1 + maxLen - log(glm)/log(10.0));
    append(res.model, gapLengthModel);
    append(res.model, "]*", 2);
    return res;
  }

  alignment D_10__IL_10(alignment x) {
    alignment res;
    append(res.sequence, '-'); append(res.model, '.');
    append(res.sequence, x.sequence); append(res.model, x.model);
    return res;
  }

  alignment D_10__ML_11(alignment x) {
    alignment res;
    append(res.sequence, '-'); append(res.model, '.');
    append(res.sequence, x.sequence); append(res.model, x.model);
    return res;
  }

  alignment D_10__D_11(alignment x) {
    alignment res;
    append(res.sequence, '-'); append(res.model, '.');
    append(res.sequence, x.sequence); append(res.model, x.model);
    return res;
  }

  alignment IL_10__IL_10(char a, alignment x) {
    alignment res;
    append(res.sequence, a); append(res.model, '-');
    append(res.sequence, x.sequence); append(res.model, x.model);
    return res;
  }

  alignment IL_10__ML_11(char a, alignment x) {
    alignment res;
    append(res.sequence, a); append(res.model, '-');
    append(res.sequence, x.sequence); append(res.model, x.model);
    return res;
  }

  alignment IL_10__D_11(char a, alignment x) {
    alignment res;
    append(res.sequence, a); append(res.model, '-');
    append(res.sequence, x.sequence); append(res.model, x.model);
    return res;
  }

  alignment ML_11__IL_11(char a, alignment x) {
    alignment res;
    append(res.sequence, a); append(res.model, '.');
    append(res.sequence, x.sequence); append(res.model, x.model);
    return res;
  }

  alignment ML_11__ML_12(char a, alignment x) {
    alignment res;
    append(res.sequence, a); append(res.model, '.');
    append(res.sequence, x.sequence); append(res.model, x.model);
    return res;
  }

  alignment ML_11__D_12(char a, alignment x) {
    alignment res;
    append(res.sequence, a); append(res.model, '.');
    append(res.sequence, x.sequence); append(res.model, x.model);
    return res;
  }

  alignment ML_11__EL(char a, Subsequence x) {
    alignment res;
    append(res.sequence, a); append(res.model, '.');
    int gapLengthSequence = size(x);
    int gapLengthModel = 1;
    float gls = size(x);
    float glm = 1.0;
    int maxLen = log(max(gls, glm))/log(10.0);
    append(res.sequence, "*[", 2);
    append(res.sequence, ' ', 1 + maxLen - log(gls)/log(10.0));
    append(res.sequence, gapLengthSequence);
    append(res.sequence, "]*", 2);
    append(res.model, "*[", 2);
    append(res.model, ' ', 1 + maxLen - log(glm)/log(10.0));
    append(res.model, gapLengthModel);
    append(res.model, "]*", 2);
    return res;
  }

  alignment D_11__IL_11(alignment x) {
    alignment res;
    append(res.sequence, '-'); append(res.model, '.');
    append(res.sequence, x.sequence); append(res.model, x.model);
    return res;
  }

  alignment D_11__ML_12(alignment x) {
    alignment res;
    append(res.sequence, '-'); append(res.model, '.');
    append(res.sequence, x.sequence); append(res.model, x.model);
    return res;
  }

  alignment D_11__D_12(alignment x) {
    alignment res;
    append(res.sequence, '-'); append(res.model, '.');
    append(res.sequence, x.sequence); append(res.model, x.model);
    return res;
  }

  alignment IL_11__IL_11(char a, alignment x) {
    alignment res;
    append(res.sequence, a); append(res.model, '-');
    append(res.sequence, x.sequence); append(res.model, x.model);
    return res;
  }

  alignment IL_11__ML_12(char a, alignment x) {
    alignment res;
    append(res.sequence, a); append(res.model, '-');
    append(res.sequence, x.sequence); append(res.model, x.model);
    return res;
  }

  alignment IL_11__D_12(char a, alignment x) {
    alignment res;
    append(res.sequence, a); append(res.model, '-');
    append(res.sequence, x.sequence); append(res.model, x.model);
    return res;
  }

  alignment ML_12__E_13(char a, alignment x) {
    alignment res;
    append(res.sequence, a); append(res.model, '.');
    append(res.sequence, x.sequence); append(res.model, x.model);
    return res;
  }

  alignment D_12__E_13(alignment x) {
    alignment res;
    append(res.sequence, '-'); append(res.model, '.');
    append(res.sequence, x.sequence); append(res.model, x.model);
    return res;
  }

  choice [alignment] h([alignment] i) {
    return i;
  }

  choice [alignment] hSelectWindow([alignment] i) {
    return i;
  }
}

algebra count auto count;

algebra enum auto enum;

grammar cm uses Algebra(axiom = left) {
	left = skipl(CHAR, left) | right # h;
	right = skipr(start, CHAR) | start # h;
  start = window(LOC, root, LOC) # h;
  root = root__global(MP_1) | root__local(local) # h;
  local = local__MP_2(MP_2) | local__MP_3(MP_3) | local__MP_4(MP_4) | local__MR_5(MR_5) | local__MP_6(MP_6) | local__MP_7(MP_7) | local__MP_8(MP_8) | local__ML_9(ML_9) | local__ML_10(ML_10) | local__ML_11(ML_11) | local__ML_12(ML_12) # h;
  MP_1 = MP_1__IL_1(CHAR, IL_1, CHAR) | MP_1__IR_1(CHAR, IR_1, CHAR) | MP_1__MP_2(CHAR, MP_2, CHAR) | MP_1__ML_2(CHAR, ML_2, CHAR) | MP_1__MR_2(CHAR, MR_2, CHAR) | MP_1__D_2(CHAR, D_2, CHAR) | MP_1__EL(CHAR, REGION0, CHAR) # h;
  IL_1 = IL_1__IL_1(CHAR, IL_1) | IL_1__IR_1(CHAR, IR_1) | IL_1__MP_2(CHAR, MP_2) | IL_1__ML_2(CHAR, ML_2) | IL_1__MR_2(CHAR, MR_2) | IL_1__D_2(CHAR, D_2) # h;
  IR_1 = IR_1__IR_1(IR_1, CHAR) | IR_1__MP_2(MP_2, CHAR) | IR_1__ML_2(ML_2, CHAR) | IR_1__MR_2(MR_2, CHAR) | IR_1__D_2(D_2, CHAR) # h;
  MP_2 = MP_2__IL_2(CHAR, IL_2, CHAR) | MP_2__IR_2(CHAR, IR_2, CHAR) | MP_2__MP_3(CHAR, MP_3, CHAR) | MP_2__ML_3(CHAR, ML_3, CHAR) | MP_2__MR_3(CHAR, MR_3, CHAR) | MP_2__D_3(CHAR, D_3, CHAR) | MP_2__EL(CHAR, REGION0, CHAR) # h;
  ML_2 = ML_2__IL_2(CHAR, IL_2) | ML_2__IR_2(CHAR, IR_2) | ML_2__MP_3(CHAR, MP_3) | ML_2__ML_3(CHAR, ML_3) | ML_2__MR_3(CHAR, MR_3) | ML_2__D_3(CHAR, D_3) # h;
  MR_2 = MR_2__IL_2(IL_2, CHAR) | MR_2__IR_2(IR_2, CHAR) | MR_2__MP_3(MP_3, CHAR) | MR_2__ML_3(ML_3, CHAR) | MR_2__MR_3(MR_3, CHAR) | MR_2__D_3(D_3, CHAR) # h;
  D_2 = D_2__IL_2(IL_2) | D_2__IR_2(IR_2) | D_2__MP_3(MP_3) | D_2__ML_3(ML_3) | D_2__MR_3(MR_3) | D_2__D_3(D_3) # h;
  IL_2 = IL_2__IL_2(CHAR, IL_2) | IL_2__IR_2(CHAR, IR_2) | IL_2__MP_3(CHAR, MP_3) | IL_2__ML_3(CHAR, ML_3) | IL_2__MR_3(CHAR, MR_3) | IL_2__D_3(CHAR, D_3) # h;
  IR_2 = IR_2__IR_2(IR_2, CHAR) | IR_2__MP_3(MP_3, CHAR) | IR_2__ML_3(ML_3, CHAR) | IR_2__MR_3(MR_3, CHAR) | IR_2__D_3(D_3, CHAR) # h;
  MP_3 = MP_3__IL_3(CHAR, IL_3, CHAR) | MP_3__IR_3(CHAR, IR_3, CHAR) | MP_3__MP_4(CHAR, MP_4, CHAR) | MP_3__ML_4(CHAR, ML_4, CHAR) | MP_3__MR_4(CHAR, MR_4, CHAR) | MP_3__D_4(CHAR, D_4, CHAR) | MP_3__EL(CHAR, REGION0, CHAR) # h;
  ML_3 = ML_3__IL_3(CHAR, IL_3) | ML_3__IR_3(CHAR, IR_3) | ML_3__MP_4(CHAR, MP_4) | ML_3__ML_4(CHAR, ML_4) | ML_3__MR_4(CHAR, MR_4) | ML_3__D_4(CHAR, D_4) # h;
  MR_3 = MR_3__IL_3(IL_3, CHAR) | MR_3__IR_3(IR_3, CHAR) | MR_3__MP_4(MP_4, CHAR) | MR_3__ML_4(ML_4, CHAR) | MR_3__MR_4(MR_4, CHAR) | MR_3__D_4(D_4, CHAR) # h;
  D_3 = D_3__IL_3(IL_3) | D_3__IR_3(IR_3) | D_3__MP_4(MP_4) | D_3__ML_4(ML_4) | D_3__MR_4(MR_4) | D_3__D_4(D_4) # h;
  IL_3 = IL_3__IL_3(CHAR, IL_3) | IL_3__IR_3(CHAR, IR_3) | IL_3__MP_4(CHAR, MP_4) | IL_3__ML_4(CHAR, ML_4) | IL_3__MR_4(CHAR, MR_4) | IL_3__D_4(CHAR, D_4) # h;
  IR_3 = IR_3__IR_3(IR_3, CHAR) | IR_3__MP_4(MP_4, CHAR) | IR_3__ML_4(ML_4, CHAR) | IR_3__MR_4(MR_4, CHAR) | IR_3__D_4(D_4, CHAR) # h;
  MP_4 = MP_4__IL_4(CHAR, IL_4, CHAR) | MP_4__IR_4(CHAR, IR_4, CHAR) | MP_4__MR_5(CHAR, MR_5, CHAR) | MP_4__D_5(CHAR, D_5, CHAR) | MP_4__EL(CHAR, REGION0, CHAR) # h;
  ML_4 = ML_4__IL_4(CHAR, IL_4) | ML_4__IR_4(CHAR, IR_4) | ML_4__MR_5(CHAR, MR_5) | ML_4__D_5(CHAR, D_5) # h;
  MR_4 = MR_4__IL_4(IL_4, CHAR) | MR_4__IR_4(IR_4, CHAR) | MR_4__MR_5(MR_5, CHAR) | MR_4__D_5(D_5, CHAR) # h;
  D_4 = D_4__IL_4(IL_4) | D_4__IR_4(IR_4) | D_4__MR_5(MR_5) | D_4__D_5(D_5) # h;
  IL_4 = IL_4__IL_4(CHAR, IL_4) | IL_4__IR_4(CHAR, IR_4) | IL_4__MR_5(CHAR, MR_5) | IL_4__D_5(CHAR, D_5) # h;
  IR_4 = IR_4__IR_4(IR_4, CHAR) | IR_4__MR_5(MR_5, CHAR) | IR_4__D_5(D_5, CHAR) # h;
  MR_5 = MR_5__IR_5(IR_5, CHAR) | MR_5__MP_6(MP_6, CHAR) | MR_5__ML_6(ML_6, CHAR) | MR_5__MR_6(MR_6, CHAR) | MR_5__D_6(D_6, CHAR) | MR_5__EL(REGION0, CHAR) # h;
  D_5 = D_5__IR_5(IR_5) | D_5__MP_6(MP_6) | D_5__ML_6(ML_6) | D_5__MR_6(MR_6) | D_5__D_6(D_6) # h;
  IR_5 = IR_5__IR_5(IR_5, CHAR) | IR_5__MP_6(MP_6, CHAR) | IR_5__ML_6(ML_6, CHAR) | IR_5__MR_6(MR_6, CHAR) | IR_5__D_6(D_6, CHAR) # h;
  MP_6 = MP_6__IL_6(CHAR, IL_6, CHAR) | MP_6__IR_6(CHAR, IR_6, CHAR) | MP_6__MP_7(CHAR, MP_7, CHAR) | MP_6__ML_7(CHAR, ML_7, CHAR) | MP_6__MR_7(CHAR, MR_7, CHAR) | MP_6__D_7(CHAR, D_7, CHAR) | MP_6__EL(CHAR, REGION0, CHAR) # h;
  ML_6 = ML_6__IL_6(CHAR, IL_6) | ML_6__IR_6(CHAR, IR_6) | ML_6__MP_7(CHAR, MP_7) | ML_6__ML_7(CHAR, ML_7) | ML_6__MR_7(CHAR, MR_7) | ML_6__D_7(CHAR, D_7) # h;
  MR_6 = MR_6__IL_6(IL_6, CHAR) | MR_6__IR_6(IR_6, CHAR) | MR_6__MP_7(MP_7, CHAR) | MR_6__ML_7(ML_7, CHAR) | MR_6__MR_7(MR_7, CHAR) | MR_6__D_7(D_7, CHAR) # h;
  D_6 = D_6__IL_6(IL_6) | D_6__IR_6(IR_6) | D_6__MP_7(MP_7) | D_6__ML_7(ML_7) | D_6__MR_7(MR_7) | D_6__D_7(D_7) # h;
  IL_6 = IL_6__IL_6(CHAR, IL_6) | IL_6__IR_6(CHAR, IR_6) | IL_6__MP_7(CHAR, MP_7) | IL_6__ML_7(CHAR, ML_7) | IL_6__MR_7(CHAR, MR_7) | IL_6__D_7(CHAR, D_7) # h;
  IR_6 = IR_6__IR_6(IR_6, CHAR) | IR_6__MP_7(MP_7, CHAR) | IR_6__ML_7(ML_7, CHAR) | IR_6__MR_7(MR_7, CHAR) | IR_6__D_7(D_7, CHAR) # h;
  MP_7 = MP_7__IL_7(CHAR, IL_7, CHAR) | MP_7__IR_7(CHAR, IR_7, CHAR) | MP_7__MP_8(CHAR, MP_8, CHAR) | MP_7__ML_8(CHAR, ML_8, CHAR) | MP_7__MR_8(CHAR, MR_8, CHAR) | MP_7__D_8(CHAR, D_8, CHAR) | MP_7__EL(CHAR, REGION0, CHAR) # h;
  ML_7 = ML_7__IL_7(CHAR, IL_7) | ML_7__IR_7(CHAR, IR_7) | ML_7__MP_8(CHAR, MP_8) | ML_7__ML_8(CHAR, ML_8) | ML_7__MR_8(CHAR, MR_8) | ML_7__D_8(CHAR, D_8) # h;
  MR_7 = MR_7__IL_7(IL_7, CHAR) | MR_7__IR_7(IR_7, CHAR) | MR_7__MP_8(MP_8, CHAR) | MR_7__ML_8(ML_8, CHAR) | MR_7__MR_8(MR_8, CHAR) | MR_7__D_8(D_8, CHAR) # h;
  D_7 = D_7__IL_7(IL_7) | D_7__IR_7(IR_7) | D_7__MP_8(MP_8) | D_7__ML_8(ML_8) | D_7__MR_8(MR_8) | D_7__D_8(D_8) # h;
  IL_7 = IL_7__IL_7(CHAR, IL_7) | IL_7__IR_7(CHAR, IR_7) | IL_7__MP_8(CHAR, MP_8) | IL_7__ML_8(CHAR, ML_8) | IL_7__MR_8(CHAR, MR_8) | IL_7__D_8(CHAR, D_8) # h;
  IR_7 = IR_7__IR_7(IR_7, CHAR) | IR_7__MP_8(MP_8, CHAR) | IR_7__ML_8(ML_8, CHAR) | IR_7__MR_8(MR_8, CHAR) | IR_7__D_8(D_8, CHAR) # h;
  MP_8 = MP_8__IL_8(CHAR, IL_8, CHAR) | MP_8__IR_8(CHAR, IR_8, CHAR) | MP_8__ML_9(CHAR, ML_9, CHAR) | MP_8__D_9(CHAR, D_9, CHAR) | MP_8__EL(CHAR, REGION0, CHAR) # h;
  ML_8 = ML_8__IL_8(CHAR, IL_8) | ML_8__IR_8(CHAR, IR_8) | ML_8__ML_9(CHAR, ML_9) | ML_8__D_9(CHAR, D_9) # h;
  MR_8 = MR_8__IL_8(IL_8, CHAR) | MR_8__IR_8(IR_8, CHAR) | MR_8__ML_9(ML_9, CHAR) | MR_8__D_9(D_9, CHAR) # h;
  D_8 = D_8__IL_8(IL_8) | D_8__IR_8(IR_8) | D_8__ML_9(ML_9) | D_8__D_9(D_9) # h;
  IL_8 = IL_8__IL_8(CHAR, IL_8) | IL_8__IR_8(CHAR, IR_8) | IL_8__ML_9(CHAR, ML_9) | IL_8__D_9(CHAR, D_9) # h;
  IR_8 = IR_8__IR_8(IR_8, CHAR) | IR_8__ML_9(ML_9, CHAR) | IR_8__D_9(D_9, CHAR) # h;
  ML_9 = ML_9__IL_9(CHAR, IL_9) | ML_9__ML_10(CHAR, ML_10) | ML_9__D_10(CHAR, D_10) | ML_9__EL(CHAR, REGION0) # h;
  D_9 = D_9__IL_9(IL_9) | D_9__ML_10(ML_10) | D_9__D_10(D_10) # h;
  IL_9 = IL_9__IL_9(CHAR, IL_9) | IL_9__ML_10(CHAR, ML_10) | IL_9__D_10(CHAR, D_10) # h;
  ML_10 = ML_10__IL_10(CHAR, IL_10) | ML_10__ML_11(CHAR, ML_11) | ML_10__D_11(CHAR, D_11) | ML_10__EL(CHAR, REGION0) # h;
  D_10 = D_10__IL_10(IL_10) | D_10__ML_11(ML_11) | D_10__D_11(D_11) # h;
  IL_10 = IL_10__IL_10(CHAR, IL_10) | IL_10__ML_11(CHAR, ML_11) | IL_10__D_11(CHAR, D_11) # h;
  ML_11 = ML_11__IL_11(CHAR, IL_11) | ML_11__ML_12(CHAR, ML_12) | ML_11__D_12(CHAR, D_12) | ML_11__EL(CHAR, REGION0) # h;
  D_11 = D_11__IL_11(IL_11) | D_11__ML_12(ML_12) | D_11__D_12(D_12) # h;
  IL_11 = IL_11__IL_11(CHAR, IL_11) | IL_11__ML_12(CHAR, ML_12) | IL_11__D_12(CHAR, D_12) # h;
  ML_12 = ML_12__E_13(CHAR, E_13) # h;
  D_12 = D_12__E_13(E_13) # h;
  E_13 = nil(EMPTY) # h;
}

instance cykpp = cm(cyk * pretty);
