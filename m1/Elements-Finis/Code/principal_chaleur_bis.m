function [UUU,MMM,KKK,C] = principal_chaleur_bis (h_param)
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
% principal_chaleur_bis :
% Cette fonction est utilisee pour calculer la solution approchee selon la valeur de h. Elle renvoie la solution approchee, les
% matrices de masse et de rigidite, et le tableau des sommets du maillage.
%
% SYNOPSIS [UUU,MMM,KKK,C] = principal_chaleur_bis (h_param)
%          
% INPUT * h_param : le parametre h a utiliser pour construire le maillage
%
% OUTPUT - UUU : Le vecteur de la solution approchee
%        - MMM,KKK : les matrices de masse et de rigidite
%        - C : le tableau Coorneu des sommets du maillage
%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

  h=h_param
  
  system(['gmsh -2 -clmax ' num2str(h) ' -clmin ' num2str(h) ' geomChaleur.geo']);
  nom_maillage = 'geomChaleur.msh' ;

  alpha = 1;
  T_Gamma = 0;
  
  % lecture du maillage et affichage
  % ---------------------------------
  [Nbpt,Nbtri,Coorneu,Refneu,Numtri,Reftri]=lecture_msh(nom_maillage);
   
  % C = zeros(size(Coorneu)(1),2);
  % C(:,1) = Coorneu(:,1);
  % C(:,2) = Coorneu(:,2);
   
  % ----------------------
  % calcul des matrices EF
  % ----------------------

  % declarations
  % ------------
  KK = sparse(Nbpt,Nbpt); % matrice de rigidite
  MM = sparse(Nbpt,Nbpt); % matrice de rigidite
  LL = zeros(Nbpt,1);     % vecteur second membre

  % boucle sur les triangles
  % ------------------------
  for l=1:Nbtri

    #A, B et C sont les sommets du triangle l
    A = Numtri(l,1);
    B = Numtri(l,2);
    C = Numtri(l,3);
    
    % Coordonnees des sommets du triangles
    S1=Coorneu(A,:);
    S2=Coorneu(B,:);
    S3=Coorneu(C,:);
    
    % calcul des matrices elementaires du triangle l 
     Kel=matK_elem(S1, S2, S3, Reftri, l);
     Mel=matM_elem(S1, S2, S3);
     
     % On fait l'assemmblage de la matrice globale en ajoutant les contributions
    % des matrices de rigidite et de masse elementaires
    for i=1:3
      I = Numtri(l,i);
      for j=1:3
        J = Numtri(l,j);
        MM(I,J)=MM(I,J)+Mel(i,j);
        KK(I,J)=KK(I,J)+Kel(i,j);
      endfor
    endfor
  end % for l

  % Matrice EF
  % -------------------------
  AA = alpha*MM+KK;

  % Calcul du second membre L
  % -------------------------
  % On calcul FF, le vecteur colonne dont la i-ï¿½me coordonnee est la valeur de f
  % sommet i
  FF=zeros(Nbpt,1);
  for i=1:Nbpt
    FF(i)=f(Coorneu(i,1),Coorneu(i,2));
  endfor

  % Le second membre LL est approxime par le produit matriciel de la matrice de masse MM
  % et du vecteur colonne FF. Cette approximation se base sur l'interpolee de f sur les sommets.
  LL = MM*FF;

  % inversion
  % ----------
    
  [tilde_AA,tilde_LL] = elimine_ex_1(AA,LL,Refneu, Nbpt, Coorneu);  

  UU = tilde_AA\tilde_LL;
  
  MMM = MM;
  UUU = UU;
  KKK = KK;
  C = Coorneu;
endfunction
