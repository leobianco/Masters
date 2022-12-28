% =====================================================
% principal_chaleur;
%
% une routine pour la mise en oeuvre des EF P1 Lagrange
% pour 
% 1) l'equation de la chaleur suivante stationnaire, avec condition de
% Dirichlet non homogene
%
% | \alpha T - div(\sigma \grad T)= S,   dans \Omega=\Omega_1 U \Omega_2
% |         T = T_\Gamma,   sur le bord
%
% ou S est la source de chaleur, T_\Gamma la temperature exterieure
% \alpha > 0 et
% \sigma = | \sigma_1 dans \Omega_1
%          | \sigma_2 dans \Omega_2
%
% 2) l'equation de la chaleur dependant du temps avec condition de 
% Dirichlet non homogene
%
% | dT/dt - div(\sigma \grad T)= S,   dans \Omega=\Omega_1 U \Omega_2 et pour tout t< t_max
% |         T = T_\Gamma,   sur le bord et pour tout t< t_max
% |         T = T_0       dans \Omega et pour t=0  
%
% ou S est la source de chaleur, T_\Gamma la temperature exterieure,
% T_0 est la valeur initiale de la temp?rature
% \alpha > 0 et
% \sigma = | \sigma_1 dans \Omega_1
%          | \sigma_2 dans \Omega_2
% =====================================================
% Donnees du probleme
% ---------------------------------
h = 0.05;
system(['gmsh -2 -clmax ' num2str(h) ' -clmin ' num2str(h) ' geomChaleur.geo']);
nom_maillage = 'geomChaleur.msh' ;

validation = 'oui';
pb_stationnaire = 'non';
pb_temporel = 'non';

if strcmp(validation,'oui')
    alpha = 1;
    T_Gamma = 0;
    var_lambda = 0;
end

if strcmp(pb_stationnaire,'oui')
    alpha = 1;
    T_Gamma = 290;
    var_lambda = 0; % Vaut 0 pour l'exercice 1 et 2.6 (i) Vaut 1 pour l'exercice 2.6 (ii).
end

if strcmp(pb_temporel,'oui')
    Tps_initial = 0;
    Tps_final = 1;
    delta_t = 0.01;
    alpha = 1/delta_t;
    N_t = (Tps_final-Tps_initial)/delta_t; % le nombre d'iterations necessaires
    T_Gamma = 280;
    var_lambda = 0; % Vaut 0 pour conditions Dirichlet, 1 conditions Fourier.
end

% lecture du maillage et affichage
% ---------------------------------
[Nbpt,Nbtri,Coorneu,Refneu,Numtri,Reftri]=lecture_msh(nom_maillage);
 
% ----------------------
% calcul des matrices EF
% ----------------------

% declarations des matrices utilisees dans les differents systemes lineaires
% ------------
KK = sparse(Nbpt,Nbpt); % matrice de rigidite
MM = sparse(Nbpt,Nbpt); % matrice de masse volumique
LL = zeros(Nbpt,1);     % vecteur second membre
SS = sparse(Nbpt,Nbpt); % matrice de masse surfacique

% boucle sur les triangles
% ------------------------
for l=1:Nbtri
  #A, B et C sont les sommets du triangle l
  A = Numtri(l,1);
  B = Numtri(l,2);
  C = Numtri(l,3);
  
  %coordonnees des sommets du triangle
  S1=Coorneu(A,:);
  S2=Coorneu(B,:);
  S3=Coorneu(C,:);
  
  %calcul des matrices elementaires du triangle l 
  Kel=matK_elem(S1, S2, S3, Reftri(l));
  Mel=matM_elem(S1, S2, S3);
   
  %On fait l'assemblage de la matrice globale en ajoutant les contributions
  %des matrices de rigidite et de masse elementaires
  for i=1:3
    I = Numtri(l,i);
    for j=1:3
      J = Numtri(l,j);
      MM(I,J)=MM(I,J)+Mel(i,j);
      KK(I,J)=KK(I,J)+Kel(i,j);
    endfor
  endfor
  
  % Calcul de la matrice de masse surfacique:
   % D'abord on doit verifier si quelque arête du triangle est dans le bord.
     % Numérotation locale: 1 --> 2; 2 --> 3; 3 --> 1; 
  for i=1:3
    if i != 3
      j = i + 1;   % Ici on a la numerotation locale de i et j.
    else 
      j = 1;
    endif
       
    I = Numtri(l, i);    % Numerotation globale.
    J = Numtri(l, j);    % Numerotation globale.
    
    % Détermination des coordonnées des sommets.
    S1 = Coorneu(I, :);
    S2 = Coorneu(J, :);
    
    % Détermination de la réference du bord.
    % J'ai crée une autre fonction qui fait ça, le code devient plus lisible.
    ref_bord = [Refneu(I), Refneu(J)];
    
    % Création de la matrice élémentaire.
    Sel = mat_elem_surface(S1, S2, ref_bord);
    
    % Assemblage de la matrice élémentaire dans la matrice globale.
    SS(I, J) = SS(I,J) + Sel(1, 2);
    SS(J, I) = SS(J,I) + Sel(2, 1);  
    SS(I, I) = SS(I,I) + Sel(1, 1);
    SS(J, J) = SS(J,J) + Sel(2, 2);
       
     endfor % for i
  
end % for l

% Matrice EF
% -------------------------
AA = alpha*MM + KK + var_lambda*SS;

% =====================================================
% =====================================================
% Pour le probleme stationnaire et la validation
% ---------------------------------
if ( strcmp(validation,'oui') || strcmp(pb_stationnaire,'oui') )
% Calcul du second membre L
% -------------------------
% On calcul FF, le vecteur colonne dont la i-eme coordonnee est la valeur de f
% au sommet i
    FF=zeros(Nbpt,1);
    for i=1:Nbpt
      FF(i)=f(Coorneu(i,1),Coorneu(i,2), alpha, T_Gamma);
    endfor
    LL_1 = MM*FF;
    
    if var_lambda == 1  %pour l'exercice 2
      GG=zeros(Nbpt,1);
      for i=1:Nbpt
        GG(i)=T_c(Coorneu(i,1),Coorneu(i,2));
      endfor
      LL_2 = MM*GG;
    elseif var_lambda == 0   %pour l'exercice 1
      LL_2 = 0;
    endif
    
    % Le second membre LL est approxime par le produit matriciel de la matrice de masse MM
    % et du vecteur colonne FF. Cette approximation se base sur l'interpolee de f sur les sommets.
    LL = LL_1 - (alpha .* LL_2);
    
    % Pseudo-elimination et inversion: 
    if var_lambda == 0  %pour l'exercice 1
      [tilde_AA,tilde_LL] = elimine_ex_1(AA,LL,Refneu, Nbpt, Coorneu);
      UU = tilde_AA\tilde_LL;
    else  %pour l'exercice 2 (pas de pseudo-elimination)
      UU = AA\LL;
    endif
    
    TT = T_Gamma + UU;
    
    %Valeur maximale de la température approchée TT
    max(TT)
    
    % validation
    % ----------
    if strcmp(validation,'oui')
      nb = 5;
      h_tab = linspace(0.05,0.2,nb);
      
      %Ces tableaux vont enregistrer les erreurs relatives L2 et H1
      erL2_tab = zeros(1,nb);
      erH1_tab = zeros(1,nb);
      
      %Calcul de chaque erreur
      for i=1:nb
        h = h_tab(i);
        [UUU,MMM,KKK,C] = principal_chaleur_bis(h);
        
        UU_exact = sin(pi*C(:,1)).*sin(pi*C(:,2));
        
        % Calcul de l'erreur L2 et H1
        erreur_L2 = sqrt((UU_exact - UUU)'*MMM*(UU_exact - UUU));
        erreur_H1 = sqrt((UU_exact - UUU)'*KKK*(UU_exact - UUU));
        
        % Calcul de l'erreur relative L2 et H1
        erreur_relative_L2 = erreur_L2/sqrt((UU_exact)'*MMM*(UU_exact));
        erreur_relative_H1 = erreur_H1/sqrt((UU_exact)'*KKK*(UU_exact));
        
        erL2_tab(1,i) = erreur_relative_L2;
        erH1_tab(1,i) = erreur_relative_H1;
      endfor
      
      %Representation de l'evolution des erreurs relatives L2 et H1
      %dans une echelle log-log
        
      %Regression lineaire pour l'erreur L2
      a = polyfit(log(h_tab),log(erL2_tab),1)(1);
      y = polyfit(log(h_tab),log(erL2_tab),1)(2);
      
      tab1 = zeros(nb,1);
      for i=1:nb
        tab1(i,1)=a*log(h_tab(i))+y;
      end
      
      %Regression lineaire pour l'erreur H1
      a_b = polyfit(log(h_tab),log(erH1_tab),1)(1);
      y_b = polyfit(log(h_tab),log(erH1_tab),1)(2);
      
      tab2 = zeros(nb,1);
      for i=1:nb
        tab2(i,1)=a_b*log(h_tab(i))+y_b;
      end
      
      %On trace les erreurs relatives
      figure;
      plot(log(h_tab),log(erL2_tab),"b*",log(h_tab),log(erH1_tab),"r*",log(h_tab),tab1,'b',log(h_tab),tab2,'r');
      
      title("Erreurs relatives L2 et H1 a l'echelle log-log");
      xlabel("log h");
      ylabel("log(erreur relative)");
      legend('erreur L2','erreur H1','Location','northwest');
    end

% visualisation
% -------------
    if strcmp(validation,'oui')
      figure;
      affiche(UU, Numtri, Coorneu, sprintf('Dirichlet - %s', nom_maillage));
    endif
    
     if strcmp(pb_stationnaire,'oui')
       figure;
      affiche(TT, Numtri, Coorneu, sprintf('Solution - %s', nom_maillage));
    endif

endif



% =====================================================
% =====================================================
% Pour le probleme temporel
% ---------------------------------
if strcmp(pb_temporel,'oui')

    % on initialise la condition initiale
    % -----------------------------------
    T_initial = condition_initiale(Coorneu(:,1),Coorneu(:,2));

	% solution a t=0
	% --------------
    UU = T_initial - T_Gamma;
    TT = T_initial;

    % visualisation
    % -------------
    figure;
    hold on;
    affiche(TT, Numtri, Coorneu, ['Temps = ', num2str(0)]);
    axis([min(Coorneu(:,1)),max(Coorneu(:,1)),min(Coorneu(:,2)),max(Coorneu(:,2)),...
        280,330,280,300]);
    hold off;
  
	% Boucle sur les pas de temps
	% ---------------------------
    for k = 1:N_t
        LL_k = zeros(Nbpt,1);
        FF_k = zeros(Nbpt,1);
        ref_bord_fourier = zeros(Nbpt, 1);
        
        % Calcul du second membre F a l instant k*delta t
        % -----------------------------------------------
		% A COMPLETER EN UTILISANT LA ROUTINE f_t.m et le terme precedent (donne par UU)
		for i=1:Nbpt
      FF_k(i) = LL_k(i) + f_t(Coorneu(i,1), Coorneu(i,2), k);
    endfor
    
    
    % Choisir lambda selon la condition aux limites.
    
    LL_k = MM*FF_k + alpha .* MM * UU;
    

		% inversion
		% ----------
		% tilde_AA ET tilde_LL_k SONT LA MATRICE EF ET LE VECTEUR SECOND MEMBRE
		% APRES PSEUDO_ELIMINATION 
		% ECRIRE LA ROUTINE elimine.m ET INSERER L APPEL A CETTE ROUTINE
		% A UN ENDROIT APPROPRIE
    if var_lambda == 1
      [tilde_AA,tilde_LL_k] = elimine_ex_3(AA,LL_k,Refneu, Nbpt, Coorneu);
    elseif var_lambda == 0
      [tilde_AA,tilde_LL_k] = elimine_ex_1(AA,LL_k,Refneu, Nbpt, Coorneu);
    endif
    UU = tilde_AA\tilde_LL_k;
    TT = T_Gamma + UU;
        
        % visualisation 
		% -------------
        pause(0.05)
        affiche(TT, Numtri, Coorneu, ['Temps = ', num2str(k*delta_t)]);
        axis([min(Coorneu(:,1)),max(Coorneu(:,1)),min(Coorneu(:,2)),max(Coorneu(:,2)),...
            280,330,280,320]);
    end
   
end

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%                                                        fin de la routine
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%2020
