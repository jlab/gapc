%{
%}

%option noyywrap nounput batch


%%

. { ECHO; }

%%

int main()
{
  yy_flex_debug = 0;
  return 0;
}
