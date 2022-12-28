% BOUCLE RECHERCHE LINÉAIRE
      
      %%%%%%% RECHERCHE LINEAIRE WOLFE (FLETCHER - LEMARÉCHAL) %%%%%%%%%%%%%%%%%
      %{
      alpha_under = 0;
      infinite = 10**(2);
      alpha_up = infinite; % on assume 10**(8) = infty
      tau_i = 1/4;        % doit être dans ]0, 1/2[
      tau_e = 1.5;        % doit être plus grand que 1
      
      % Pas de Fletcher
      e_min = -5;
      Delta_k = 10**(-2) .* (e_avant - e_min);
      alpha_init = (-2) .* Delta_k / (dot(g + ae.' * lme + ai.'* lmi, d));     % pas initial
      alpha_k = alpha_init;
      
      for i = 0:options.maxit            
        
       x_pas = x_k + alpha_k .* d;
       lme_pas = lme + alpha_k .* (lambda.eqlin - lme); 
       lmi_pas = lmi + alpha_k .* (lambda.ineqlin - lmi);
       [e, ce, ci, g, ae, ai, hl_, indic_sortie] = feval(simul,4,x_pas,lme_pas, lmi_pas);
       appels_sim += 1;
        
       if e > e_avant + omega_1 .* alpha_k .* dot(g_avant + ae_avant.' * lme + ai_avant.' * lmi,d)
         alpha_up = alpha_k;
         alpha_k = unifrnd((1 - tau_i) .* alpha_under + tau_i .* alpha_up, tau_i .* alpha_up + (1 - tau_i) .* alpha_up);
       else
         if dot(g + ae.'* lme_pas + ai.' * lmi_pas, d) >= omega_2 .* dot(g_avant + ae_avant.' * lme + ai_avant.' * lmi,d)
          alpha_k
          break
         
         else
          alpha_under = alpha_k;
          if alpha_up >= infinite
            alpha_k = unifrnd(tau_e .* alpha_under, infinite);
          else
            alpha_k = unifrnd((1 - tau_i) .* alpha_under + tau_i .* alpha_up, tau_i .* alpha_up + (1 - tau_i) .* alpha_up);
          endif
         endif
       endif
        
       %} 