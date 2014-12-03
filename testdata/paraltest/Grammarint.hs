module Grammarint where
import ADPTriCombinators
import Data.Array (bounds)
import Type
grammar alg inp = axiom s_0 where
  ((f_IL_1,f_IR_2,f_ML_3,f_D_4,f_IL_5,f_ML_6,f_D_7,f_IL_8,f_ML_9,f_D_10,f_IL_11,f_ML_12,f_D_13,f_IL_14,f_ML_15,f_D_16,f_IL_17,f_MR_18,f_D_19,f_IR_20,f_MR_21,f_D_22,f_IR_23,f_MR_24,f_D_25,f_IR_26,f_MR_27,f_D_28,f_IR_29,f_MR_30,f_D_31,f_IR_32,f_MP_33,f_ML_34,f_MR_35,f_D_36,f_IL_37,f_IR_38,f_MP_39,f_ML_40,f_MR_41,f_D_42,f_IL_43,f_IR_44,f_MP_45,f_ML_46,f_MR_47,f_D_48,f_IL_49,f_IR_50,f_MP_51,f_ML_52,f_MR_53,f_D_54,f_IL_55,f_IR_56,f_MP_57,f_ML_58,f_MR_59,f_D_60),(f_IL_61,f_IR_62,f_MP_63,f_ML_64,f_MR_65,f_D_66,f_IL_67,f_IR_68,f_ML_69,f_D_70,f_IL_71,f_ML_72,f_D_73,f_IL_74,f_ML_75,f_D_76,f_IL_77,f_ML_78,f_D_79,f_E_81,nil,h)) = alg

  s_0 = tabulated (
    (f_IL_1 (-6506)) <<< base -~~ il_1 |||
    (f_IR_2 (-6713)) <<< ir_2 ~~- base |||
    (f_ML_3 (-73)) <<< base -~~ ml_3 |||
    (f_D_4 (-5127)) <<< d_4 ... h)
  il_1 = tabulated (
    (f_IL_1 (-1686)) <<< base -~~ il_1 |||
    (f_IR_2 (-2369)) <<< ir_2 ~~- base |||
    (f_ML_3 (-1117)) <<< base -~~ ml_3 |||
    (f_D_4 (-4855)) <<< d_4 ... h)
  ir_2 = tabulated (
    (f_IR_2 (-1442)) <<< ir_2 ~~- base |||
    (f_ML_3 (-798)) <<< base -~~ ml_3 |||
    (f_D_4 (-4142)) <<< d_4 ... h)
  ml_3 = tabulated (
    (f_IL_5 (-7559)) <<< base -~~ il_5 |||
    (f_ML_6 (-27)) <<< base -~~ ml_6 |||
    (f_D_7 (-6213)) <<< d_7 ... h)
  d_4 = tabulated (
    (f_IL_5 (-6174)) <<< base -~~ il_5 |||
    (f_ML_6 (-1687)) <<< base -~~ ml_6 |||
    (f_D_7 (-566)) <<< d_7 ... h)
  il_5 = tabulated (
    (f_IL_5 (-1442)) <<< base -~~ il_5 |||
    (f_ML_6 (-798)) <<< base -~~ ml_6 |||
    (f_D_7 (-4142)) <<< d_7 ... h)
  ml_6 = tabulated (
    (f_IL_8 (-7559)) <<< base -~~ il_8 |||
    (f_ML_9 (-27)) <<< base -~~ ml_9 |||
    (f_D_10 (-6213)) <<< d_10 ... h)
  d_7 = tabulated (
    (f_IL_8 (-6174)) <<< base -~~ il_8 |||
    (f_ML_9 (-1687)) <<< base -~~ ml_9 |||
    (f_D_10 (-566)) <<< d_10 ... h)
  il_8 = tabulated (
    (f_IL_8 (-1442)) <<< base -~~ il_8 |||
    (f_ML_9 (-798)) <<< base -~~ ml_9 |||
    (f_D_10 (-4142)) <<< d_10 ... h)
  ml_9 = tabulated (
    (f_IL_11 (-7559)) <<< base -~~ il_11 |||
    (f_ML_12 (-27)) <<< base -~~ ml_12 |||
    (f_D_13 (-6213)) <<< d_13 ... h)
  d_10 = tabulated (
    (f_IL_11 (-6174)) <<< base -~~ il_11 |||
    (f_ML_12 (-1687)) <<< base -~~ ml_12 |||
    (f_D_13 (-566)) <<< d_13 ... h)
  il_11 = tabulated (
    (f_IL_11 (-1442)) <<< base -~~ il_11 |||
    (f_ML_12 (-798)) <<< base -~~ ml_12 |||
    (f_D_13 (-4142)) <<< d_13 ... h)
  ml_12 = tabulated (
    (f_IL_14 (-7559)) <<< base -~~ il_14 |||
    (f_ML_15 (-27)) <<< base -~~ ml_15 |||
    (f_D_16 (-6213)) <<< d_16 ... h)
  d_13 = tabulated (
    (f_IL_14 (-6174)) <<< base -~~ il_14 |||
    (f_ML_15 (-1687)) <<< base -~~ ml_15 |||
    (f_D_16 (-566)) <<< d_16 ... h)
  il_14 = tabulated (
    (f_IL_14 (-1442)) <<< base -~~ il_14 |||
    (f_ML_15 (-798)) <<< base -~~ ml_15 |||
    (f_D_16 (-4142)) <<< d_16 ... h)
  ml_15 = tabulated (
    (f_IL_17 (-7979)) <<< base -~~ il_17 |||
    (f_MR_18 (-24)) <<< mr_18 ~~- base |||
    (f_D_19 (-6297)) <<< d_19 ... h)
  d_16 = tabulated (
    (f_IL_17 (-5620)) <<< base -~~ il_17 |||
    (f_MR_18 (-734)) <<< mr_18 ~~- base |||
    (f_D_19 (-1403)) <<< d_19 ... h)
  il_17 = tabulated (
    (f_IL_17 (-1925)) <<< base -~~ il_17 |||
    (f_MR_18 (-554)) <<< mr_18 ~~- base |||
    (f_D_19 (-4164)) <<< d_19 ... h)
  mr_18 = tabulated (
    (f_IR_20 (-7979)) <<< ir_20 ~~- base |||
    (f_MR_21 (-24)) <<< mr_21 ~~- base |||
    (f_D_22 (-6297)) <<< d_22 ... h)
  d_19 = tabulated (
    (f_IR_20 (-6390)) <<< ir_20 ~~- base |||
    (f_MR_21 (-1568)) <<< mr_21 ~~- base |||
    (f_D_22 (-620)) <<< d_22 ... h)
  ir_20 = tabulated (
    (f_IR_20 (-1925)) <<< ir_20 ~~- base |||
    (f_MR_21 (-554)) <<< mr_21 ~~- base |||
    (f_D_22 (-4164)) <<< d_22 ... h)
  mr_21 = tabulated (
    (f_IR_23 (-7979)) <<< ir_23 ~~- base |||
    (f_MR_24 (-24)) <<< mr_24 ~~- base |||
    (f_D_25 (-6297)) <<< d_25 ... h)
  d_22 = tabulated (
    (f_IR_23 (-6390)) <<< ir_23 ~~- base |||
    (f_MR_24 (-1568)) <<< mr_24 ~~- base |||
    (f_D_25 (-620)) <<< d_25 ... h)
  ir_23 = tabulated (
    (f_IR_23 (-1925)) <<< ir_23 ~~- base |||
    (f_MR_24 (-554)) <<< mr_24 ~~- base |||
    (f_D_25 (-4164)) <<< d_25 ... h)
  mr_24 = tabulated (
    (f_IR_26 (-7979)) <<< ir_26 ~~- base |||
    (f_MR_27 (-24)) <<< mr_27 ~~- base |||
    (f_D_28 (-6297)) <<< d_28 ... h)
  d_25 = tabulated (
    (f_IR_26 (-6390)) <<< ir_26 ~~- base |||
    (f_MR_27 (-1568)) <<< mr_27 ~~- base |||
    (f_D_28 (-620)) <<< d_28 ... h)
  ir_26 = tabulated (
    (f_IR_26 (-1925)) <<< ir_26 ~~- base |||
    (f_MR_27 (-554)) <<< mr_27 ~~- base |||
    (f_D_28 (-4164)) <<< d_28 ... h)
  mr_27 = tabulated (
    (f_IR_29 (-7979)) <<< ir_29 ~~- base |||
    (f_MR_30 (-24)) <<< mr_30 ~~- base |||
    (f_D_31 (-6297)) <<< d_31 ... h)
  d_28 = tabulated (
    (f_IR_29 (-6390)) <<< ir_29 ~~- base |||
    (f_MR_30 (-1568)) <<< mr_30 ~~- base |||
    (f_D_31 (-620)) <<< d_31 ... h)
  ir_29 = tabulated (
    (f_IR_29 (-1925)) <<< ir_29 ~~- base |||
    (f_MR_30 (-554)) <<< mr_30 ~~- base |||
    (f_D_31 (-4164)) <<< d_31 ... h)
  mr_30 = tabulated (
    (f_IR_32 (-6746)) <<< ir_32 ~~- base |||
    (f_MP_33 (-50)) <<< base -~~ mp_33 ~~- base |||
    (f_ML_34 (-6562)) <<< base -~~ ml_34 |||
    (f_MR_35 (-6774)) <<< mr_35 ~~- base |||
    (f_D_36 (-7666)) <<< d_36 ... h)
  d_31 = tabulated (
    (f_IR_32 (-5352)) <<< ir_32 ~~- base |||
    (f_MP_33 (-707)) <<< base -~~ mp_33 ~~- base |||
    (f_ML_34 (-2978)) <<< base -~~ ml_34 |||
    (f_MR_35 (-4409)) <<< mr_35 ~~- base |||
    (f_D_36 (-2404)) <<< d_36 ... h)
  ir_32 = tabulated (
    (f_IR_32 (-2408)) <<< ir_32 ~~- base |||
    (f_MP_33 (-496)) <<< base -~~ mp_33 ~~- base |||
    (f_ML_34 (-5920)) <<< base -~~ ml_34 |||
    (f_MR_35 (-4086)) <<< mr_35 ~~- base |||
    (f_D_36 (-5193)) <<< d_36 ... h)
  mp_33 = tabulated (
    (f_IL_37 (-8954)) <<< base -~~ il_37 |||
    (f_IR_38 (-8894)) <<< ir_38 ~~- base |||
    (f_MP_39 (-23)) <<< base -~~ mp_39 ~~- base |||
    (f_ML_40 (-7670)) <<< base -~~ ml_40 |||
    (f_MR_41 (-7950)) <<< mr_41 ~~- base |||
    (f_D_42 (-8345)) <<< d_42 ... h)
  ml_34 = tabulated (
    (f_IL_37 (-6250)) <<< base -~~ il_37 |||
    (f_IR_38 (-6596)) <<< ir_38 ~~- base |||
    (f_MP_39 (-1310)) <<< base -~~ mp_39 ~~- base |||
    (f_ML_40 (-1004)) <<< base -~~ ml_40 |||
    (f_MR_41 (-6446)) <<< mr_41 ~~- base |||
    (f_D_42 (-3975)) <<< d_42 ... h)
  mr_35 = tabulated (
    (f_IL_37 (-6988)) <<< base -~~ il_37 |||
    (f_IR_38 (-5717)) <<< ir_38 ~~- base |||
    (f_MP_39 (-1625)) <<< base -~~ mp_39 ~~- base |||
    (f_ML_40 (-5695)) <<< base -~~ ml_40 |||
    (f_MR_41 (-829)) <<< mr_41 ~~- base |||
    (f_D_42 (-3908)) <<< d_42 ... h)
  d_36 = tabulated (
    (f_IL_37 (-9049)) <<< base -~~ il_37 |||
    (f_IR_38 (-7747)) <<< ir_38 ~~- base |||
    (f_MP_39 (-3544)) <<< base -~~ mp_39 ~~- base |||
    (f_ML_40 (-4226)) <<< base -~~ ml_40 |||
    (f_MR_41 (-4244)) <<< mr_41 ~~- base |||
    (f_D_42 (-319)) <<< d_42 ... h)
  il_37 = tabulated (
    (f_IL_37 (-2579)) <<< base -~~ il_37 |||
    (f_IR_38 (-2842)) <<< ir_38 ~~- base |||
    (f_MP_39 (-760)) <<< base -~~ mp_39 ~~- base |||
    (f_ML_40 (-4497)) <<< base -~~ ml_40 |||
    (f_MR_41 (-5274)) <<< mr_41 ~~- base |||
    (f_D_42 (-4934)) <<< d_42 ... h)
  ir_38 = tabulated (
    (f_IR_38 (-2408)) <<< ir_38 ~~- base |||
    (f_MP_39 (-496)) <<< base -~~ mp_39 ~~- base |||
    (f_ML_40 (-5920)) <<< base -~~ ml_40 |||
    (f_MR_41 (-4086)) <<< mr_41 ~~- base |||
    (f_D_42 (-5193)) <<< d_42 ... h)
  mp_39 = tabulated (
    (f_IL_43 (-8954)) <<< base -~~ il_43 |||
    (f_IR_44 (-8894)) <<< ir_44 ~~- base |||
    (f_MP_45 (-23)) <<< base -~~ mp_45 ~~- base |||
    (f_ML_46 (-7670)) <<< base -~~ ml_46 |||
    (f_MR_47 (-7950)) <<< mr_47 ~~- base |||
    (f_D_48 (-8345)) <<< d_48 ... h)
  ml_40 = tabulated (
    (f_IL_43 (-6250)) <<< base -~~ il_43 |||
    (f_IR_44 (-6596)) <<< ir_44 ~~- base |||
    (f_MP_45 (-1310)) <<< base -~~ mp_45 ~~- base |||
    (f_ML_46 (-1004)) <<< base -~~ ml_46 |||
    (f_MR_47 (-6446)) <<< mr_47 ~~- base |||
    (f_D_48 (-3975)) <<< d_48 ... h)
  mr_41 = tabulated (
    (f_IL_43 (-6988)) <<< base -~~ il_43 |||
    (f_IR_44 (-5717)) <<< ir_44 ~~- base |||
    (f_MP_45 (-1625)) <<< base -~~ mp_45 ~~- base |||
    (f_ML_46 (-5695)) <<< base -~~ ml_46 |||
    (f_MR_47 (-829)) <<< mr_47 ~~- base |||
    (f_D_48 (-3908)) <<< d_48 ... h)
  d_42 = tabulated (
    (f_IL_43 (-9049)) <<< base -~~ il_43 |||
    (f_IR_44 (-7747)) <<< ir_44 ~~- base |||
    (f_MP_45 (-3544)) <<< base -~~ mp_45 ~~- base |||
    (f_ML_46 (-4226)) <<< base -~~ ml_46 |||
    (f_MR_47 (-4244)) <<< mr_47 ~~- base |||
    (f_D_48 (-319)) <<< d_48 ... h)
  il_43 = tabulated (
    (f_IL_43 (-2579)) <<< base -~~ il_43 |||
    (f_IR_44 (-2842)) <<< ir_44 ~~- base |||
    (f_MP_45 (-760)) <<< base -~~ mp_45 ~~- base |||
    (f_ML_46 (-4497)) <<< base -~~ ml_46 |||
    (f_MR_47 (-5274)) <<< mr_47 ~~- base |||
    (f_D_48 (-4934)) <<< d_48 ... h)
  ir_44 = tabulated (
    (f_IR_44 (-2408)) <<< ir_44 ~~- base |||
    (f_MP_45 (-496)) <<< base -~~ mp_45 ~~- base |||
    (f_ML_46 (-5920)) <<< base -~~ ml_46 |||
    (f_MR_47 (-4086)) <<< mr_47 ~~- base |||
    (f_D_48 (-5193)) <<< d_48 ... h)
  mp_45 = tabulated (
    (f_IL_49 (-8954)) <<< base -~~ il_49 |||
    (f_IR_50 (-8894)) <<< ir_50 ~~- base |||
    (f_MP_51 (-23)) <<< base -~~ mp_51 ~~- base |||
    (f_ML_52 (-7670)) <<< base -~~ ml_52 |||
    (f_MR_53 (-7950)) <<< mr_53 ~~- base |||
    (f_D_54 (-8345)) <<< d_54 ... h)
  ml_46 = tabulated (
    (f_IL_49 (-6250)) <<< base -~~ il_49 |||
    (f_IR_50 (-6596)) <<< ir_50 ~~- base |||
    (f_MP_51 (-1310)) <<< base -~~ mp_51 ~~- base |||
    (f_ML_52 (-1004)) <<< base -~~ ml_52 |||
    (f_MR_53 (-6446)) <<< mr_53 ~~- base |||
    (f_D_54 (-3975)) <<< d_54 ... h)
  mr_47 = tabulated (
    (f_IL_49 (-6988)) <<< base -~~ il_49 |||
    (f_IR_50 (-5717)) <<< ir_50 ~~- base |||
    (f_MP_51 (-1625)) <<< base -~~ mp_51 ~~- base |||
    (f_ML_52 (-5695)) <<< base -~~ ml_52 |||
    (f_MR_53 (-829)) <<< mr_53 ~~- base |||
    (f_D_54 (-3908)) <<< d_54 ... h)
  d_48 = tabulated (
    (f_IL_49 (-9049)) <<< base -~~ il_49 |||
    (f_IR_50 (-7747)) <<< ir_50 ~~- base |||
    (f_MP_51 (-3544)) <<< base -~~ mp_51 ~~- base |||
    (f_ML_52 (-4226)) <<< base -~~ ml_52 |||
    (f_MR_53 (-4244)) <<< mr_53 ~~- base |||
    (f_D_54 (-319)) <<< d_54 ... h)
  il_49 = tabulated (
    (f_IL_49 (-2579)) <<< base -~~ il_49 |||
    (f_IR_50 (-2842)) <<< ir_50 ~~- base |||
    (f_MP_51 (-760)) <<< base -~~ mp_51 ~~- base |||
    (f_ML_52 (-4497)) <<< base -~~ ml_52 |||
    (f_MR_53 (-5274)) <<< mr_53 ~~- base |||
    (f_D_54 (-4934)) <<< d_54 ... h)
  ir_50 = tabulated (
    (f_IR_50 (-2408)) <<< ir_50 ~~- base |||
    (f_MP_51 (-496)) <<< base -~~ mp_51 ~~- base |||
    (f_ML_52 (-5920)) <<< base -~~ ml_52 |||
    (f_MR_53 (-4086)) <<< mr_53 ~~- base |||
    (f_D_54 (-5193)) <<< d_54 ... h)
  mp_51 = tabulated (
    (f_IL_55 (-8954)) <<< base -~~ il_55 |||
    (f_IR_56 (-8894)) <<< ir_56 ~~- base |||
    (f_MP_57 (-23)) <<< base -~~ mp_57 ~~- base |||
    (f_ML_58 (-7670)) <<< base -~~ ml_58 |||
    (f_MR_59 (-7950)) <<< mr_59 ~~- base |||
    (f_D_60 (-8345)) <<< d_60 ... h)
  ml_52 = tabulated (
    (f_IL_55 (-6250)) <<< base -~~ il_55 |||
    (f_IR_56 (-6596)) <<< ir_56 ~~- base |||
    (f_MP_57 (-1310)) <<< base -~~ mp_57 ~~- base |||
    (f_ML_58 (-1004)) <<< base -~~ ml_58 |||
    (f_MR_59 (-6446)) <<< mr_59 ~~- base |||
    (f_D_60 (-3975)) <<< d_60 ... h)
  mr_53 = tabulated (
    (f_IL_55 (-6988)) <<< base -~~ il_55 |||
    (f_IR_56 (-5717)) <<< ir_56 ~~- base |||
    (f_MP_57 (-1625)) <<< base -~~ mp_57 ~~- base |||
    (f_ML_58 (-5695)) <<< base -~~ ml_58 |||
    (f_MR_59 (-829)) <<< mr_59 ~~- base |||
    (f_D_60 (-3908)) <<< d_60 ... h)
  d_54 = tabulated (
    (f_IL_55 (-9049)) <<< base -~~ il_55 |||
    (f_IR_56 (-7747)) <<< ir_56 ~~- base |||
    (f_MP_57 (-3544)) <<< base -~~ mp_57 ~~- base |||
    (f_ML_58 (-4226)) <<< base -~~ ml_58 |||
    (f_MR_59 (-4244)) <<< mr_59 ~~- base |||
    (f_D_60 (-319)) <<< d_60 ... h)
  il_55 = tabulated (
    (f_IL_55 (-2579)) <<< base -~~ il_55 |||
    (f_IR_56 (-2842)) <<< ir_56 ~~- base |||
    (f_MP_57 (-760)) <<< base -~~ mp_57 ~~- base |||
    (f_ML_58 (-4497)) <<< base -~~ ml_58 |||
    (f_MR_59 (-5274)) <<< mr_59 ~~- base |||
    (f_D_60 (-4934)) <<< d_60 ... h)
  ir_56 = tabulated (
    (f_IR_56 (-2408)) <<< ir_56 ~~- base |||
    (f_MP_57 (-496)) <<< base -~~ mp_57 ~~- base |||
    (f_ML_58 (-5920)) <<< base -~~ ml_58 |||
    (f_MR_59 (-4086)) <<< mr_59 ~~- base |||
    (f_D_60 (-5193)) <<< d_60 ... h)
  mp_57 = tabulated (
    (f_IL_61 (-8954)) <<< base -~~ il_61 |||
    (f_IR_62 (-8894)) <<< ir_62 ~~- base |||
    (f_MP_63 (-23)) <<< base -~~ mp_63 ~~- base |||
    (f_ML_64 (-7670)) <<< base -~~ ml_64 |||
    (f_MR_65 (-7950)) <<< mr_65 ~~- base |||
    (f_D_66 (-8345)) <<< d_66 ... h)
  ml_58 = tabulated (
    (f_IL_61 (-6250)) <<< base -~~ il_61 |||
    (f_IR_62 (-6596)) <<< ir_62 ~~- base |||
    (f_MP_63 (-1310)) <<< base -~~ mp_63 ~~- base |||
    (f_ML_64 (-1004)) <<< base -~~ ml_64 |||
    (f_MR_65 (-6446)) <<< mr_65 ~~- base |||
    (f_D_66 (-3975)) <<< d_66 ... h)
  mr_59 = tabulated (
    (f_IL_61 (-6988)) <<< base -~~ il_61 |||
    (f_IR_62 (-5717)) <<< ir_62 ~~- base |||
    (f_MP_63 (-1625)) <<< base -~~ mp_63 ~~- base |||
    (f_ML_64 (-5695)) <<< base -~~ ml_64 |||
    (f_MR_65 (-829)) <<< mr_65 ~~- base |||
    (f_D_66 (-3908)) <<< d_66 ... h)
  d_60 = tabulated (
    (f_IL_61 (-9049)) <<< base -~~ il_61 |||
    (f_IR_62 (-7747)) <<< ir_62 ~~- base |||
    (f_MP_63 (-3544)) <<< base -~~ mp_63 ~~- base |||
    (f_ML_64 (-4226)) <<< base -~~ ml_64 |||
    (f_MR_65 (-4244)) <<< mr_65 ~~- base |||
    (f_D_66 (-319)) <<< d_66 ... h)
  il_61 = tabulated (
    (f_IL_61 (-2579)) <<< base -~~ il_61 |||
    (f_IR_62 (-2842)) <<< ir_62 ~~- base |||
    (f_MP_63 (-760)) <<< base -~~ mp_63 ~~- base |||
    (f_ML_64 (-4497)) <<< base -~~ ml_64 |||
    (f_MR_65 (-5274)) <<< mr_65 ~~- base |||
    (f_D_66 (-4934)) <<< d_66 ... h)
  ir_62 = tabulated (
    (f_IR_62 (-2408)) <<< ir_62 ~~- base |||
    (f_MP_63 (-496)) <<< base -~~ mp_63 ~~- base |||
    (f_ML_64 (-5920)) <<< base -~~ ml_64 |||
    (f_MR_65 (-4086)) <<< mr_65 ~~- base |||
    (f_D_66 (-5193)) <<< d_66 ... h)
  mp_63 = tabulated (
    (f_IL_67 (-6506)) <<< base -~~ il_67 |||
    (f_IR_68 (-6713)) <<< ir_68 ~~- base |||
    (f_ML_69 (-73)) <<< base -~~ ml_69 |||
    (f_D_70 (-5127)) <<< d_70 ... h)
  ml_64 = tabulated (
    (f_IL_67 (-3758)) <<< base -~~ il_67 |||
    (f_IR_68 (-3940)) <<< ir_68 ~~- base |||
    (f_ML_69 (-507)) <<< base -~~ ml_69 |||
    (f_D_70 (-2670)) <<< d_70 ... h)
  mr_65 = tabulated (
    (f_IL_67 (-4809)) <<< base -~~ il_67 |||
    (f_IR_68 (-3838)) <<< ir_68 ~~- base |||
    (f_ML_69 (-1706)) <<< base -~~ ml_69 |||
    (f_D_70 (-766)) <<< d_70 ... h)
  d_66 = tabulated (
    (f_IL_67 (-4568)) <<< base -~~ il_67 |||
    (f_IR_68 (-4250)) <<< ir_68 ~~- base |||
    (f_ML_69 (-2265)) <<< base -~~ ml_69 |||
    (f_D_70 (-520)) <<< d_70 ... h)
  il_67 = tabulated (
    (f_IL_67 (-1686)) <<< base -~~ il_67 |||
    (f_IR_68 (-2369)) <<< ir_68 ~~- base |||
    (f_ML_69 (-1117)) <<< base -~~ ml_69 |||
    (f_D_70 (-4855)) <<< d_70 ... h)
  ir_68 = tabulated (
    (f_IR_68 (-1442)) <<< ir_68 ~~- base |||
    (f_ML_69 (-798)) <<< base -~~ ml_69 |||
    (f_D_70 (-4142)) <<< d_70 ... h)
  ml_69 = tabulated (
    (f_IL_71 (-7559)) <<< base -~~ il_71 |||
    (f_ML_72 (-27)) <<< base -~~ ml_72 |||
    (f_D_73 (-6213)) <<< d_73 ... h)
  d_70 = tabulated (
    (f_IL_71 (-6174)) <<< base -~~ il_71 |||
    (f_ML_72 (-1687)) <<< base -~~ ml_72 |||
    (f_D_73 (-566)) <<< d_73 ... h)
  il_71 = tabulated (
    (f_IL_71 (-1442)) <<< base -~~ il_71 |||
    (f_ML_72 (-798)) <<< base -~~ ml_72 |||
    (f_D_73 (-4142)) <<< d_73 ... h)
  ml_72 = tabulated (
    (f_IL_74 (-7559)) <<< base -~~ il_74 |||
    (f_ML_75 (-27)) <<< base -~~ ml_75 |||
    (f_D_76 (-6213)) <<< d_76 ... h)
  d_73 = tabulated (
    (f_IL_74 (-6174)) <<< base -~~ il_74 |||
    (f_ML_75 (-1687)) <<< base -~~ ml_75 |||
    (f_D_76 (-566)) <<< d_76 ... h)
  il_74 = tabulated (
    (f_IL_74 (-1442)) <<< base -~~ il_74 |||
    (f_ML_75 (-798)) <<< base -~~ ml_75 |||
    (f_D_76 (-4142)) <<< d_76 ... h)
  ml_75 = tabulated (
    (f_IL_77 (-7559)) <<< base -~~ il_77 |||
    (f_ML_78 (-27)) <<< base -~~ ml_78 |||
    (f_D_79 (-6213)) <<< d_79 ... h)
  d_76 = tabulated (
    (f_IL_77 (-6174)) <<< base -~~ il_77 |||
    (f_ML_78 (-1687)) <<< base -~~ ml_78 |||
    (f_D_79 (-566)) <<< d_79 ... h)
  il_77 = tabulated (
    (f_IL_77 (-1442)) <<< base -~~ il_77 |||
    (f_ML_78 (-798)) <<< base -~~ ml_78 |||
    (f_D_79 (-4142)) <<< d_79 ... h)
  ml_78 = tabulated (
    (f_E_81 (-6)) <<< e_81 ... h)
  d_79 = tabulated (
    (f_E_81 (-4)) <<< e_81 ... h)
  e_81 = (nil <<< empty)
  z = mk inp
  (_,n) = bounds z
  base = achar' z
  axiom = axiom' n
  tabulated = table n

