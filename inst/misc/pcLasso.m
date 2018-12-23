"
c
c                          Lariat (8/12/18)
c
c
c call pclasso(no,ni,x,y,w,theta,ng,mg,aa,ne,nx,nlam,ulam,thr,maxit,verbose,ao,ia,kin,nlp,jerr)
c   no = number of observations
c   ni = number of predictor variables
c   x(no,ni) = predictor data matrix (all columns centered)
c   y(no) = response vector (centered)
c   w(no)= observation weights
c   theta = group regularization parameter
c   ng = number of groups
c   mg(ng+1) = vector points to groups in aa
c   aa(ni,max_group_size) = group covarience matricies in order
c   ne = maximum number of variables allowed to enter largest model
c        (stopping criterion)
c   nx = maximum number of variables allowed to enter all models
c        along path (memory allocation, nx > ne).
c   nlam = number of lambda values
c   ulam(nlam) = user supplied lambda values in descending order
c   thr = convergence threshold for each lamda solution.
c      iterations stop when the maximum reduction in the criterion value
c      as a result of each parameter update over a single pass
c      is less than thr times the null criterion value.
c      (suggested value, thr=1.0e-5)
c   maxit = maximum allowed number of passes over the data for all lambda
c      values (suggested values, maxit = 100000)
c  verbose:  should progress be printed?   0/1= no/yes
c
c output:
c
c   ao(nx,nlam) = compressed coefficient values for each solution
c   ia(nx) = pointers to compressed coefficients
c   kin(nlam) = number of compressed coefficients for each solution
c   nlp = actual number of passes over the data for all lambda values
c   jerr = error flag:
c      jerr  = 0 => no error
c      jerr > 0 => fatal error - no output returned
c         jerr < 7777 => memory allocation error
C      jerr < 0 => non fatal error - partial output:
c         Solutions for larger lamdas (1:(k-1)) returned.
c         jerr = -k => convergence for kth lamda value not reached
c            after maxit (see above) iterations.
c         jerr = -10000-k => number of non zero coefficients along path
c            exceeds nx (see above) at kth lamda value.
c
c
c
c least-squares utility routines:
c
c
c uncompress coefficient vector for particular solution:
c
c call uncomp(ni,ao,ia,kin,a)
c
c input:
c
c    ni = total number of predictor variables
c    ao(nx) = compressed coefficient values for the solution
c    ia(nx) = pointers to compressed coefficients
c    kin = number of compressed coefficients for the solution
c
c output:
c
c    a(ni) =  uncompressed coefficient vector
c             referencing original variables
c
c
c evaluate linear model from compressed coefficients and
c uncompressed predictor matrix:
c
c call modval(ao,ia,kin,n,x,f);
c
c input:
c
c    ao(nx) = compressed coefficient values for a solution
c    ia(nx) = pointers to compressed coefficients
c    kin = number of compressed coefficients for solution
c    n = number of predictor vectors (observations)
c    x(n,ni) = full (uncompressed) predictor matrix
c
c output:
c
c    f(n) = model predictions
c
c
"
subroutine pclasso(no,ni,x,y,w,theta,ng,mg,aa,ne,nx,nlam,ulam,
     thr,maxit,verbose,ao,ia,kin,nlp,jerr);
real y(no),x(no,ni),w(no),aa(ni,*),ulam(nlam),ao(nx,nlam);
integer ia(nx),kin(nlam),mg(ng+1),verbose;
%fortran
      real, dimension (:), allocatable :: a,r,sx,s,uu,wt
      integer, dimension (:), allocatable :: mm
%mortran
allocate(a(1:ni),stat=jerr);
allocate(r(1:no),stat=ierr); jerr=jerr+ierr;
allocate(mm(1:ni),stat=ierr); jerr=jerr+ierr;
allocate(sx(1:ni),stat=ierr); jerr=jerr+ierr;
allocate(s(1:ni),stat=ierr); jerr=jerr+ierr;
allocate(uu(1:ni),stat=ierr); jerr=jerr+ierr;
allocate(wt(1:no),stat=ierr); jerr=jerr+ierr;
if(jerr.ne.0) return;

wt=no*w/sum(w);


k=1; a=0.0; uu=0; mm=0; s=0.0; /nlp,nin/=0; r=y; sw=sum(w);
<j=1,ni; sx(j)=sum(wt*x(:,j)**2);> ysq=sum(w*y**2)/sw;

rsq0=1.0;
<m=1,nlam;  alm=ulam(m);
%fortran  
      if(verbose.eq.1) call dblepr("lambda=",-1,alm,1);
%mortran
  loop < nlp=nlp+1;
      <kg=1,ng; lg=mg(kg+1)-mg(kg);
         <k=1,lg; j=k+mg(kg)-1; gj=dot_product(wt*r,x(:,j));
            aj=a(j); u=gj+sx(j)*aj; aajj=aa(j,j-mg(kg)+1);
   
            s(j)=uu(j)-aajj*aj;
            u=u-theta*s(j); v=abs(u)-alm; a(j)=0.0;
            if(v.gt.0.0) a(j)=sign(v,u)/(sx(j)+theta*aajj);
        
            if(a(j).eq.aj) next;
  
            if mm(j).eq.0 < nin=nin+1; if(nin.gt.nx) exit;
               mm(j)=nin; ia(nin)=j;
            >
            del=a(j)-aj; r=r-del*x(:,j);
            <l=1,lg; jl=l+mg(kg)-1; uu(jl)=uu(jl)+del*aa(jl,k);>
     
         >        
      >
      if(nin.gt.nx) exit;
      if nlp.gt.maxit < jerr=-m; return;>
      rsq=sum(w*r**2)/(ysq*sw);
      if(abs(rsq0-rsq).lt.thr) exit;
      rsq0=rsq;
    
   >
   if nin.gt.nx < jerr=-10000-m;  exit;>
   if(nin.gt.0) ao(1:nin,m)=a(ia(1:nin)); kin(m)=nin;
   me=0; <j=1,nin; if(ao(j,m).ne.0.0) me=me+1;> if(me.gt.ne) exit;

>
deallocate(a,r,mm,sx,s,uu);
return;
end;
subroutine uncomp(ni,ao,ia,kin,a);
real ao(*),a(ni); integer ia(*);
a=0.0; if(kin.gt.0) a(ia(1:kin))=ao(1:kin);
return;
end;
subroutine modval(ao,ia,kin,n,x,f);
real ao(kin),x(n,*),f(n); integer ia(kin);
f=a0; if(kin.le.0) return;
<i=1,n; f(i)=f(i)+dot_product(ao(1:kin),x(i,ia(1:kin)));>
return;
end;

"
c
c                          Log-pclasso (8/12/18)
c         For binomial loss
c
c call logpclassp(no,ni,x,y,w,theta,ng,mg,aa,ne,nx,nlam,ulam,thr,maxit,a0,ao,ia,kin,nlp,jerr)
c   no = number of observations
c   ni = number of predictor variables
c   x(no,ni) = predictor data matrix (all columns centered)
c   y(no) = response vector (centered)
c   w(no)= observation weights
c   theta = group regularization parameter
c   ng = number of groups
c   mg(ng+1) = vector points to groups in aa
c   aa(ni,max_group_size) = group covarience matricies in order
c   ne = maximum number of variables allowed to enter largest model
c        (stopping criterion)
c   nx = maximum number of variables allowed to enter all models
c        along path (memory allocation, nx > ne).
c   nlam = number of lambda values
c   ulam(nlam) = user supplied lamda values in descending order
c   thr = convergence threshold for each lamda solution.
c      iterations stop when the maximum reduction in the criterion value
c      as a result of each parameter update over a single pass
c      is less than thr times the null criterion value.
c      (suggested value, thr=1.0e-5)
c   maxit = maximum allowed number of passes over the data for all lambda
c      values (suggested values, maxit = 100000)
c   verbose: should  progress be printed?  0/1= no/yes
c
c output:
c
c   a0(nlam)  estimated intercepts for each model
c   ao(nx,nlam) = compressed coefficient values for each solution
c   ia(nx) = pointers to compressed coefficients
c   kin(nlam) = number of compressed coefficients for each solution
c   nlp = actual number of passes over the data for all lamda values
c   jerr = error flag:
c      jerr  = 0 => no error
c      jerr > 0 => fatal error - no output returned
c         jerr < 7777 => memory allocation error
C      jerr < 0 => non fatal error - partial output:
c         Solutions for larger lamdas (1:(k-1)) returned.
c         jerr = -k => convergence for kth lamda value not reached
c            after maxit (see above) iterations.
c         jerr = -10000-k => number of non zero coefficients along path
c            exceeds nx (see above) at kth lamda value.
c
c
c
c least-squares utility routines:
c
c
c uncompress coefficient vector for particular solution:
c
c call uncomp(ni,ao,ia,kin,a)
c
c input:
c
c    ni = total number of predictor variables
c    ao(nx) = compressed coefficient values for the solution
c    ia(nx) = pointers to compressed coefficients
c    kin = number of compressed coefficients for the solution
c
c output:
c
c    a(ni) =  uncompressed coefficient vector
c             referencing original variables
c
c
c evaluate linear model from compressed coefficients and
c uncompressed predictor matrix:
c
c call modval(ao,ia,kin,n,x,f);
c
c input:
c
c    ao(nx) = compressed coefficient values for a solution
c    ia(nx) = pointers to compressed coefficients
c    kin = number of compressed coefficients for solution
c    n = number of predictor vectors (observations)
c    x(n,ni) = full (uncompressed) predictor matrix
c
c output:
c
c    f(n) = model predictions
c
c
"
subroutine logpclasso(no,ni,x,y,w,theta,ng,mg,aa,ne,nx,nlam,ulam,
     thr,maxit,verbose,a0,ao,ia,kin,nlp,jerr);
real y(no),x(no,ni),w(no),a0(nlam),aa(ni,*),ulam(nlam),ao(nx,nlam);
integer ia(nx),kin(nlam),mg(ng+1),verbose;
%fortran
      real, dimension (:), allocatable :: a,r,sx,s,uu,wt,
     *zz,pr
      real, dimension (:), allocatable :: eta,eta0,warg    
      integer, dimension (:), allocatable :: mm
%mortran
allocate(a(1:ni),stat=jerr);
allocate(r(1:no),stat=ierr); jerr=jerr+ierr;
allocate(mm(1:ni),stat=ierr); jerr=jerr+ierr;
allocate(sx(1:ni),stat=ierr); jerr=jerr+ierr;
allocate(s(1:ni),stat=ierr); jerr=jerr+ierr;
allocate(uu(1:ni),stat=ierr); jerr=jerr+ierr;
allocate(wt(1:no),stat=ierr); jerr=jerr+ierr;
allocate(zz(1:no),stat=ierr); jerr=jerr+ierr;
allocate(pr(1:no),stat=ierr); jerr=jerr+ierr;
allocate(eta0(1:no),stat=ierr); jerr=jerr+ierr;
allocate(eta(1:no),stat=ierr); jerr=jerr+ierr;
allocate(warg(1:no),stat=ierr); jerr=jerr+ierr;
if(jerr.ne.0) return;
    
xminw=.0001;
del2thr=.01;


wt=no*w/sum(w);

 uu=0; mm=0; s=0.0; /nlp,nin/=0; r=y; sw=sum(w);
<j=1,ni; sx(j)=sum(wt*x(:,j)**2);> ysq=sum(w*y**2)/sw;

az=0.0; a=0.0; it=0; /nlp,nth,kp,kth/=0; r=y;
warg=w; zz=y; eta=0; pr=1/(1+exp(-eta));

 w=0.25;
    
w=warg*w;
w=no*w/sum(w); sw=sum(w); a0=0; mlam=0;

rsq0=1.0;
<m=1,nlam;  alm=ulam(m); 

%fortran  
      if(verbose.eq.1) call dblepr("lambda=",-1,alm,1);
%mortran

  loop <"beginning of IRLS loop  first few lines below are NEW"
      loop < nlp=nlp+1; dlx=0.0;
       aj=az; del=dot_product(w,r)/sw;  "intercept"
               az=az+del; r=r-del;
      <kg=1,ng; lg=mg(kg+1)-mg(kg);
         <k=1,lg; j=k+mg(kg)-1; gj=dot_product(wt*r,x(:,j));
            aj=a(j); u=gj+sx(j)*aj; aajj=aa(j,j-mg(kg)+1);
   
            s(j)=uu(j)-aajj*aj;
            u=u-theta*s(j); v=abs(u)-alm; a(j)=0.0;
            if(v.gt.0.0) a(j)=sign(v,u)/(sx(j)+theta*aajj);
        
            if(a(j).eq.aj) next;
  
            if mm(j).eq.0 < nin=nin+1; if(nin.gt.nx) exit;
               mm(j)=nin; ia(nin)=j;
            >
            del=a(j)-aj; r=r-del*x(:,j);
            <l=1,lg; jl=l+mg(kg)-1; uu(jl)=uu(jl)+del*aa(jl,k);>
        >
        > "end of kg=1,ng loop"
    
      
      if(nin.gt.nx) exit;
      if nlp.gt.maxit < jerr=-m; return;>
      rsq=sum(w*r**2)/(ysq*sw);
    
      if(abs(rsq0-rsq).lt.thr) exit;
      rsq0=rsq;
       >"end of  loop statement" 
   
 
     "NEW block"
           eta0=eta; eta=zz-r;      
      del2=sum(abs(eta-eta0))/no;
      pr=1/(1+exp(-eta)); 
     

        w=0.25;
     
 
       zz=eta+(y-pr)/w;
 

      w=warg*w; w=no*w/sum(w); sw=sum(w);
      a0(m)=az; r=zz-eta;    
 

   > until del2.lt.del2thr;  " end of IRLS loop end of  NEW block"
 
      
    
   
   if nin.gt.nx < jerr=-10000-m;  exit;>
   if(nin.gt.0) ao(1:nin,m)=a(ia(1:nin)); kin(m)=nin;
   me=0; <j=1,nin; if(ao(j,m).ne.0.0) me=me+1;> if(me.gt.ne) exit;

>"end of m=1,nlam loop"

deallocate(a,r,mm,sx,s,uu);
return;
end;



%%
