
signature Bill(alphabet, answer) {

  
  choice [answer] h([answer]);
}


grammar bill uses Bill (axiom=formula) {

  formula = .[
                int h = 1;
                t_0_k1 = h;
            ]. {
              pk(region, region, region) .{
                pk(region[i, i+h], front[i+h+1] )
              }.
            }  ;

}
