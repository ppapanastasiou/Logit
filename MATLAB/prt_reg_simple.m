function prt_reg_simple(results,vnames,fid)
% PURPOSE: Prints output using regression results structures
%---------------------------------------------------
% USAGE: prt_reg(results,vnames,fid)
% Where: results = a structure returned by a regression 
%        vnames  = an optional vector of variable names
%        fid     = optional file-id for printing results to a file
%                  (defaults to the MATLAB command window)
%--------------------------------------------------- 
%  NOTES: e.g. vnames = strvcat('y','const','x1','x2');
%         e.g. fid = fopen('ols.out','wr');
%  use prt_reg(results,[],fid) to print to a file with no vnames               
% --------------------------------------------------
%  RETURNS: nothing, just prints the regression results
% --------------------------------------------------
% SEE ALSO: prt, plt
%---------------------------------------------------   

% written by:
% James P. LeSage, Dept of Economics
% University of Toledo
% 2801 W. Bancroft St,
% Toledo, OH 43606
% jpl@jpl.econ.utoledo.edu

if ~isstruct(results)
 error('prt_reg requires structure argument');
elseif nargin == 1
 nflag = 0; fid = 1;
elseif nargin == 2
 fid = 1; nflag = 1;
elseif nargin == 3
 nflag = 0;
 [vsize junk] = size(vnames); % user may supply a blank argument
   if vsize > 0
   nflag = 1;          
   end;
else
 error('Wrong # of arguments to prt_reg');
end;

nobs = results.nobs;
nvar = results.nvar;

% make up some generic variable names
Vname = 'Variable';
 for i=1:nvar
    tmp = ['variable ',num2str(i)];
    Vname = strvcat(Vname,tmp);
 end;
if (nflag == 1) % the user supplied variable names
[tst_n nsize] = size(vnames);
 if tst_n ~= nvar+1
 fprintf(fid,'Wrong # of variable names in prt_reg -- check vnames argument \n');
 fprintf(fid,'will use generic variable names \n');
 nflag = 0;
 else,
Vname = 'Variable';
 for i=1:nvar
    Vname = strvcat(Vname,vnames(i+1,:));
 end;
 end; % end of if-else
end; % end of nflag issue

switch results.meth


case {'ols','hwhite','nwest','olsrs'} % <=================== ols,white,nwest,olsrs regressions

fprintf(fid,'\n');
if strcmp(results.meth,'ols')
fprintf(fid,'Ordinary Least-squares Estimates \n');
elseif strcmp(results.meth,'hwhite')
fprintf(fid,'White Heteroscedastic Consistent Estimates \n');
elseif strcmp(results.meth,'nwest')
   fprintf(fid,'Newey-West hetero/serial Consistent Estimates \n');
elseif strcmp(results.meth,'olsrs')
fprintf(fid,'Restricted Least-squares Estimates \n');
end;

if (nflag == 1)
fprintf(fid,'Dependent Variable = %16s \n',vnames(1,:));
end;
fprintf(fid,'R-squared      = %9.4f \n',results.rsqr);
fprintf(fid,'Rbar-squared   = %9.4f \n',results.rbar);
fprintf(fid,'sigma^2        = %9.4f \n',results.sige);
fprintf(fid,'Durbin-Watson  = %9.4f \n',results.dw);
fprintf(fid,'Nobs, Nvars    = %6d,%6d \n',results.nobs,results.nvar);
fprintf(fid,'***************************************************************\n');

% <=================== end of ols,white, newey-west case


case {'logit','probit'}  % <=================== logit,probit regressions

fprintf(fid,'\n');
if strcmp(results.meth,'logit')
fprintf(fid,'Logit Maximum Likelihood Estimates \n');
elseif strcmp(results.meth,'probit')
fprintf(fid,'Probit Maximum Likelihood Estimates \n');
end;

if (nflag == 1)
fprintf(fid,'Dependent Variable = %16s \n',vnames(1,:));
end;
fprintf(fid,'McFadden R-squared     = %9.4f \n',results.r2mf);
fprintf(fid,'Estrella R-squared     = %9.4f \n',results.rsqr);
fprintf(fid,'LR-ratio, 2*(Lu-Lr)    = %9.4f \n',results.lratio);
fprintf(fid,'LR p-value             = %9.4f \n',1-chis_prb(results.lratio,nvar-1));
fprintf(fid,'Log-Likelihood         = %9.4f \n',results.lik);
fprintf(fid,'# of iterations        = %6d   \n',results.iter);
fprintf(fid,'Convergence criterion  = %16.8g \n',results.convg);
fprintf(fid,'Nobs, Nvars            = %6d,%6d \n',results.nobs,results.nvar);
fprintf(fid,'# of 0''s, # of 1''s     = %6d,%6d \n',results.zip,results.one);
fprintf(fid,'***************************************************************\n');

%<=================== end of logit,probit

otherwise
error('results structure not known by prt_reg function');
end;

% now print coefficient estimates, t-statistics and probabilities
tout = tdis_prb(results.tstat,nobs-nvar); % find t-stat probabilities
tmp = [results.beta results.tstat tout];  % matrix to be printed
% column labels for printing results
bstring = 'Coefficient'; tstring = 't-statistic'; pstring = 't-probability';
cnames = strvcat(bstring,tstring,pstring);
in.cnames = cnames;
in.rnames = Vname;
in.fmt = '%16.6f';
in.fid = fid;
mprint(tmp,in);


