
function [tilde_AA,tilde_LL] = elimine_ex_1(AA,LL,Refneu, Nbpt, Coorneu)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% elimine :
% Procédure de pseudo-élimination des noeuds i correspondant à Refneu(i)=1
%
% SYNOPSIS tilde_AA,tilde_LL = elimine (AA,LL,Refneu)
%          
% INPUT * AA,LL,Refneu : AA et LL vont subir la pseudo-élimination selon les valeurs
%                        de Refneu
%
% OUTPUT - tilde_AA,tilde_LL: matrices pseudo-éliminées
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
Nbpt = size(Refneu)(1);

vecteur_nul = zeros(Nbpt,1);


% Pour le premier exercice.
for i=1:Nbpt  %on parcourt les sommets
  if Refneu(i,1)==1  %si la référence du noeud est 1, on applique la pseudo-élimination
    AA(i,:)=vecteur_nul;  %on élimine la ligne i de AA
    AA(:,i)=vecteur_nul;  %on élimine la colonne i de AA
    AA(i,i)=1;  %on place le coefficient diagonal i de AA à 1
    LL(i)=0;  %on élimine la i-ème composante de LL
  endif
endfor


%tilde_AA,tilde_LL = AA,LL;
tilde_AA = AA;
tilde_LL = LL;

endfunction
