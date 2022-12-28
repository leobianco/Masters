function fun_trace (x,y)
  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  % fun_trace :
  % Trace la chaîne à partir des coordonnées des points
  %
  % SYNOPSIS fun_trace (x,y)
  %          
  % INPUT * x,y : coordonnées des points
  %%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
  
  figure();
  plot(x,y,'-*')
  title('Tracé de la chaîne');
endfunction
