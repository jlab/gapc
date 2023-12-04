> module RNAshapesMfe.AlgebraType where

Algebra type:

> type Canonical_Algebra alph1 alph2 closed answer pf_closed =
>  (alph1 -> closed -> closed,  --sadd
>   closed -> closed -> closed, --cadd
>   closed -> pf_closed -> closed, --cadd'
>   closed -> closed -> pf_closed, --cadd''
>   closed -> pf_closed -> pf_closed, --cadd'''
>   closed -> alph1 -> pf_closed -> closed, --ambd
>   closed -> alph1 -> pf_closed -> pf_closed, --ambd'
>   () -> closed,               --nil
>   () -> pf_closed,            --nil'
>   alph1 -> closed -> closed,  --edl
>   closed -> alph1 -> closed,  --edr
>   alph1 -> closed -> alph1 -> closed,  --edlr
>   closed -> closed,                    --drem   
>   closed -> closed,                    --is
>   alph1 -> closed -> alph1 -> closed,  --sr
>   alph1 -> alph1 -> alph2 -> alph1 -> alph1 -> closed,  --hl
>   alph1 -> alph1 -> alph2 -> alph1 -> alph1 -> closed,  --hlChar
>   alph1 -> alph1 -> closed -> alph1 -> alph1 -> closed, --sp
>   alph2 -> closed -> closed ,                           --bl         
>   closed -> alph2 -> closed,                            --br
>   alph2 -> closed -> alph2 -> closed,                   --il
>   alph1 -> alph1 -> answer -> alph1 -> alph1 -> closed,                   --ml
>   alph1 -> alph1 -> answer -> alph1 -> alph1 -> alph1 -> closed,          --mldr
>   alph1 -> alph1 -> answer -> alph1 -> alph1 -> alph1 -> closed,          --mladr
>   alph1 -> alph1 -> alph1 -> answer -> alph1 -> alph1 -> alph1 -> closed, --mldlr
>   alph1 -> alph1 -> alph1 -> answer -> alph1 -> alph1 -> alph1 -> closed, --mladlr
>   alph1 -> alph1 -> alph1 -> answer -> alph1 -> alph1 -> alph1 -> closed, --mldladr
>   alph1 -> alph1 -> alph1 -> answer -> alph1 -> alph1 -> alph1 -> closed, --mladldr
>   alph1 -> alph1 -> alph1 -> answer -> alph1 -> alph1 -> closed,          --mldl
>   alph1 -> alph1 -> alph1 -> answer -> alph1 -> alph1 -> closed,          --mladl
>   answer -> alph2 -> answer,  --addss
>   alph2 -> closed -> answer,  --ssadd
>   pf_closed -> closed, --trafo
>   closed -> answer,                   --incl
>   answer -> answer -> answer,  --combine
>   answer -> answer -> answer,  --lcombine
>   answer -> answer -> answer,  --lcombine'
>   answer -> answer -> answer,  --rcombine
>   answer -> answer -> answer,  --rcombine'
>   answer -> answer -> answer,  --lrcombine
>   answer -> alph1 -> answer -> answer, --acomb
>   answer -> alph1 -> answer -> answer, --lacomb
>   answer -> alph1 -> answer -> answer, --lacomb'
>   answer -> alph1 -> answer -> answer, --racomb
>   answer -> alph1 -> answer -> answer, --racomb'
>   answer -> alph1 -> answer -> answer, --lracomb
>   [closed] -> [closed], --h
>   [answer] -> [answer], --h_i
>   [closed] -> [closed], --h_l
>   [pf_closed] -> [pf_closed]  --h_s
>   )
