R=function(u,mod)
{
  if(mod=="R0")
  {
    return(u)
  }
  if(mod=="R1")
  {
    return(quadBezier(u,delta=0.4,delta2=0.4,stage=2))
  }
  if(mod=="R2")
  {
    return(quadBezier(u,delta=0.4,delta2=0.8,stage=2))
  }
  if(mod=="R3")
  {
    return(quadBezier(u,delta=0.8,delta2=0.4,stage=2))
  }
  if(mod=="R4")
  {
    return(quadBezier(u,delta=0.8,delta2=0.8,stage=2))
  }
  if(mod=="R5")
  {
    return(RLS(u,delta=0.25))
  }
  if(mod=="R6")
  {
    return(quadBezier(u,delta=-0.4,delta2=0.8,stage=2))
  }
  if(mod=="R7")
  {
    return(pnorm(qnorm(u,0,1/2)))
  }
  if(mod=="R8")
  {
    return(pnorm(qnorm(u,0,2)))
  }
  #################delta_c_#######################
  if(mod=="delta_c_0")
  {
    return(RLS(u,delta=0.5))
  }
  if(mod=="delta_c_1")
  {
    return(RLS(u,delta=0.45))
  }
  if(mod=="delta_c_2")
  {
    return(RLS(u,delta=0.4))
  }
  if(mod=="delta_c_3")
  {
    return(RLS(u,delta=0.35))
  }
  if(mod=="delta_c_4")
  {
    return(RLS(u,delta=0.3))
  }
  if(mod=="delta_c_5")
  {
    return(RLS(u,delta=0.25))
  }
  if(mod=="delta_c_6")
  {
    return(RLS(u,delta=0.2))
  }
  if(mod=="delta_c_7")
  {
    return(RLS(u,delta=0.15))
  }
  if(mod=="delta_c_8")
  {
    return(RLS(u,delta=0.1))
  }
  if(mod=="delta_c_9")
  {
    return(RLS(u,delta=0.05))
  }
  #################delta_b_#######################
  if(mod=="delta_b_0")
  {
    return(u)
  }
  if(mod=="delta_b_1")
  {
    return(pRotaODC(u,pi/4*(19/20)))
  }
  if(mod=="delta_b_2")
  {
    return(pRotaODC(u,pi/4*(18/20)))
  }
  if(mod=="delta_b_3")
  {
    return(pRotaODC(u,pi/4*(17/20)))
  }
  if(mod=="delta_b_4")
  {
    return(pRotaODC(u,pi/4*(16/20)))
  }
  if(mod=="delta_b_5")
  {
    return(pRotaODC(u,pi/4*(15/20)))
  }
  if(mod=="delta_b_6")
  {
    return(pRotaODC(u,pi/4*(14/20)))
  }
  if(mod=="delta_b_7")
  {
    return(pRotaODC(u,pi/4*(13/20)))
  }
  if(mod=="delta_b_8")
  {
    return(pRotaODC(u,pi/4*(12/20)))
  }
  if(mod=="delta_b_9")
  {
    return(pRotaODC(u,pi/4*(11/20)))
  }
  #################delta_a_#######################
  if(mod=="delta_a_0")
  {
    return(u)
  }
  if(mod=="delta_a_1")
  {
    return(pRotaODC(u,pi/4+pi/4*(1/10)))
  }
  if(mod=="delta_a_2")
  {
    return(pRotaODC(u,pi/4+pi/4*(2/10)))
  }
  if(mod=="delta_a_3")
  {
    return(pRotaODC(u,pi/4+pi/4*(3/10)))
  }
  if(mod=="delta_a_4")
  {
    return(pRotaODC(u,pi/4+pi/4*(4/10)))
  }
  if(mod=="delta_a_5")
  {
    return(pRotaODC(u,pi/4+pi/4*(5/10)))
  }
  if(mod=="delta_a_6")
  {
    return(pRotaODC(u,pi/4+pi/4*(6/10)))
  }
  if(mod=="delta_a_7")
  {
    return(pRotaODC(u,pi/4+pi/4*(7/10)))
  }
  if(mod=="delta_a_8")
  {
    return(pRotaODC(u,pi/4+pi/4*(8/10)))
  }
  if(mod=="delta_a_9")
  {
    return(pRotaODC(u,pi/4+pi/4*(9/10)))
  }
  #################delta_d_#######################
  delta=0.06; endpoint=delta*10
  if(mod=="delta_d_0")
  {
    return(u)
  }
  if(mod=="delta_d_1")
  {
    return(pSTODC(u,endpoint-1*delta,endpoint))
  }
  if(mod=="delta_d_2")
  {
    return(pSTODC(u,endpoint-2*delta,endpoint))
  }
  if(mod=="delta_d_3")
  {
    return(pSTODC(u,endpoint-3*delta,endpoint))
  }
  if(mod=="delta_d_4")
  {
    return(pSTODC(u,endpoint-4*delta,endpoint))
  }
  if(mod=="delta_d_5")
  {
    return(pSTODC(u,endpoint-5*delta,endpoint))
  }
  if(mod=="delta_d_6")
  {
    return(pSTODC(u,endpoint-6*delta,endpoint))
  }
  if(mod=="delta_d_7")
  {
    return(pSTODC(u,endpoint-7*delta,endpoint))
  }
  if(mod=="delta_d_8")
  {
    return(pSTODC(u,endpoint-8*delta,endpoint))
  }
  if(mod=="delta_d_9")
  {
    return(pSTODC(u,endpoint-9*delta,endpoint))
  }
}

Data.Generate=function(m,n,mod="R0")
{
  if(mod=="R0")
  {
    X=runif(m)
    Y=runif(n)
  }
  if(mod=="R1")
  {
    X=rqB(m,delta=0.4,delta2=0.4,stage=2)
    Y=runif(n)
  }
  if(mod=="R2")
  {
    X=rqB(m,delta=0.4,delta2=0.8,stage=2)
    Y=runif(n)
  }
  if(mod=="R3")
  {
    X=rqB(m,delta=0.8,delta2=0.4,stage=2)
    Y=runif(n)
  }
  if(mod=="R4")
  {
    X=rqB(m,delta=0.8,delta2=0.8,stage=2)
    Y=runif(n)
  }
  if(mod=="R5")
  {
    X=RIS(runif(m),delta=0.25)
    Y=runif(n)
  }
  if(mod=="R6")
  {
    X=rqB(m, delta=-0.4, delta2=0.8, stage=2)
    Y=runif(n)
  }
  if(mod=="R7")
  {
    X=rnorm(m)
    Y=rnorm(n,0,1/2)
  }
  if(mod=="R8")
  {
    X=rnorm(m)
    Y=rnorm(n,0,2)
  }
  ##############################delta_c_####################
  if(mod=="delta_c_0")
  {
    X=RIS(runif(m),delta=0.5)
    Y=runif(n)
  }
  if(mod=="delta_c_1")
  {
    X=RIS(runif(m),delta=0.45)
    Y=runif(n)
  }
  if(mod=="delta_c_2")
  {
    X=RIS(runif(m),delta=0.4)
    Y=runif(n)
  }
  if(mod=="delta_c_3")
  {
    X=RIS(runif(m),delta=0.35)
    Y=runif(n)
  }
  if(mod=="delta_c_4")
  {
    X=RIS(runif(m),delta=0.3)
    Y=runif(n)
  }
  if(mod=="delta_c_5")
  {
    X=RIS(runif(m),delta=0.25)
    Y=runif(n)
  }
  if(mod=="delta_c_6")
  {
    X=RIS(runif(m),delta=0.2)
    Y=runif(n)
  }
  if(mod=="delta_c_7")
  {
    X=RIS(runif(m),delta=0.15)
    Y=runif(n)
  }
  if(mod=="delta_c_8")
  {
    X=RIS(runif(m),delta=0.1)
    Y=runif(n)
  }
  if(mod=="delta_c_9")
  {
    X=RIS(runif(m),delta=0.05)
    Y=runif(n)
  }
  #########################delta_b_#####################
  if(mod=="delta_b_0")
  {
    X=runif(m)
    Y=runif(n)
  }
  if(mod=="delta_b_1")
  {
    X=rRotaODC(m,pi/4*(19/20))
    Y=runif(n)
  }
  if(mod=="delta_b_2")
  {
    X=rRotaODC(m,pi/4*(18/20))
    Y=runif(n)
  }
  if(mod=="delta_b_3")
  {
    X=rRotaODC(m,pi/4*(17/20))
    Y=runif(n)
  }
  if(mod=="delta_b_4")
  {
    X=rRotaODC(m,pi/4*(16/20))
    Y=runif(n)
  }
  if(mod=="delta_b_5")
  {
    X=rRotaODC(m,pi/4*(15/20))
    Y=runif(n)
  }
  if(mod=="delta_b_6")
  {
    X=rRotaODC(m,pi/4*(14/20))
    Y=runif(n)
  }
  if(mod=="delta_b_7")
  {
    X=rRotaODC(m,pi/4*(13/20))
    Y=runif(n)
  }
  if(mod=="delta_b_8")
  {
    X=rRotaODC(m,pi/4*(12/20))
    Y=runif(n)
  }
  if(mod=="delta_b_9")
  {
    X=rRotaODC(m,pi/4*(11/20))
    Y=runif(n)
  }
  #########################delta_a_#####################
  if(mod=="delta_a_0")
  {
    X=runif(m)
    Y=runif(n)
  }
  if(mod=="delta_a_1")
  {
    X=rRotaODC(m,pi/4+pi/4*(1/10))
    Y=runif(n)
  }
  if(mod=="delta_a_2")
  {
    X=rRotaODC(m,pi/4+pi/4*(2/10))
    Y=runif(n)
  }
  if(mod=="delta_a_3")
  {
    X=rRotaODC(m,pi/4+pi/4*(3/10))
    Y=runif(n)
  }
  if(mod=="delta_a_4")
  {
    X=rRotaODC(m,pi/4+pi/4*(4/10))
    Y=runif(n)
  }
  if(mod=="delta_a_5")
  {
    X=rRotaODC(m,pi/4+pi/4*(5/10))
    Y=runif(n)
  }
  if(mod=="delta_a_6")
  {
    X=rRotaODC(m,pi/4+pi/4*(6/10))
    Y=runif(n)
  }
  if(mod=="delta_a_7")
  {
    X=rRotaODC(m,pi/4+pi/4*(7/10))
    Y=runif(n)
  }
  if(mod=="delta_a_8")
  {
    X=rRotaODC(m,pi/4+pi/4*(8/10))
    Y=runif(n)
  }
  if(mod=="delta_a_9")
  {
    X=rRotaODC(m,pi/4+pi/4*(9/10))
    Y=runif(n)
  }
  #########################delta_d_#####################
  delta=0.06; endpoint=delta*10
  if(mod=="delta_d_0")
  {
    X=runif(m)
    Y=runif(n)
  }
  if(mod=="delta_d_1")
  {
    X=rSTODC(m,endpoint-1*delta,endpoint)
    Y=runif(n)
  }
  if(mod=="delta_d_2")
  {
    X=rSTODC(m,endpoint-2*delta,endpoint)
    Y=runif(n)
  }
  if(mod=="delta_d_3")
  {
    X=rSTODC(m,endpoint-3*delta,endpoint)
    Y=runif(n)
  }
  if(mod=="delta_d_4")
  {
    X=rSTODC(m,endpoint-4*delta,endpoint)
    Y=runif(n)
  }
  if(mod=="delta_d_5")
  {
    X=rSTODC(m,endpoint-5*delta,endpoint)
    Y=runif(n)
  }
  if(mod=="delta_d_6")
  {
    X=rSTODC(m,endpoint-6*delta,endpoint)
    Y=runif(n)
  }
  if(mod=="delta_d_7")
  {
    X=rSTODC(m,endpoint-7*delta,endpoint)
    Y=runif(n)
  }
  if(mod=="delta_d_8")
  {
    X=rSTODC(m,endpoint-8*delta,endpoint)
    Y=runif(n)
  }
  if(mod=="delta_d_9")
  {
    X=rSTODC(m,endpoint-9*delta,endpoint)
    Y=runif(n)
  }
  return(list(X=X,Y=Y))
}

#################################################
###  quadratic Bezier distribution and sampling
#################################################


quadBezierCoeff = function(delta=0.1){
  r = (1+delta)/(1-delta)
  cc = 2*delta/(1+delta)
  m = (1-delta)/2
  a = 4/(5*r+4)
  b = (5-4*cc)*r/(5*r+4)
  return(list(a=a,b=b,r=r,cc=cc,m=m))
}
tf = function(u=u,delta=0){
  r = (1+delta)/(1-delta)
  cc = 2*delta/(1+delta)
  m = (1-delta)/2
  a = 4/(5*r+4)
  b = (5-4*cc)*r/(5*r+4)
  tf = 1/(a+b-2*m)*(a-m+sqrt(m^2-a*b+(a+b-2*m)*u))
  return(tf)
}
ftf = function(u=u,a=a,m=m,b=b){
  ftf = 1/(a+b-2*m)*(a-m+sqrt(m^2-a*b+(a+b-2*m)*u))
  return(ftf)
}
dtf = function(u=u,delta=0){
  r = (1+delta)/(1-delta)
  cc = 2*delta/(1+delta)
  m = (1-delta)/2
  a = 4/(5*r+4)
  b = (5-4*cc)*r/(5*r+4)
  dtf = 1/(a+b-2*m)*(m^2-a*b+(a+b-2*m)*u)^(-1/2)*(a+b-2*m)/2
  return(dtf)
}

#################### 2 stages 

quadBezier = function(u,delta=0.1,delta2=1,stage=2){
  n = length(u)
  
  if (delta!=0){
    if (stage==2){
      r = (1+delta)/(1-delta)
      cc = 2*delta/(1+delta)
      m = (1-delta)/2
      a = 4/(5*r+4)
      b = (5-4*cc)*r/(5*r+4)
      quadBezier = array(,n)
      aa1 = (1-delta2)*3/4
      mm1 = (1-delta2)
      bb1 = (1-delta2) + (a+(1-a)*(1-delta2)-(1-delta2))/4
      aa2 = a+(1-a)*(1-delta2)
      mm2 = ((r-1)-mm1*r*(1-r))/(r^2-1)
      bb2 = b+(1-b)*(1-delta2)
      for (i in 1:n){
        if( 0  <= u[i] & u[i]<= aa1){
          quadBezier[i] = u[i]
        }
        if( aa1<  u[i] & u[i]<  bb1){
          w1  = ftf(u=u[i],a=aa1,m=mm1,b=bb1)
          quadBezier[i] = (1-w1)^2*aa1 + 2*(1-w1)*w1*mm1 + w1^2*(mm1+r*(bb1-mm1))
        }
        if( bb1<= u[i] & u[i]<= aa2){
          quadBezier[i] = mm1+r*(u[i]-mm1)
        }
        if( aa2<  u[i] & u[i]< bb2){
          w2  = ftf(u=u[i],a=aa2,m=mm2,b=bb2)
          quadBezier[i] = (1-w2)^2*(mm1+r*(aa2-mm1))+ 
            2*(1-w2)*w2*(cc + mm2/r) + 
            w2^2*(cc + bb2/r)
        }
        if( bb2<= u[i] & u[i]<= 1){
          quadBezier[i] = cc + u[i]/r
        }
        
      }
    }
    
    if (stage==1){
      r = (1+delta)/(1-delta)
      cc = 2*delta/(1+delta)
      m = (1-delta)/2
      a = 4/(5*r+4)
      b = (5-4*cc)*r/(5*r+4)
      quadBezier = array(,n)
      aa1 = (1-delta2)*3/4
      mm1 = (1-delta2)
      bb1 = (1-delta2) + (a+(1-a)*(1-delta2)-(1-delta2))/4
      aa2 = a+(1-a)*(1-delta2)
      mm2 = ((r-1)-mm1*r*(1-r))/(r^2-1)
      bb2 = b+(1-b)*(1-delta2)
      for (i in 1:n){
        if( 0<=  u[i] & u[i]<  bb1){
          w1  = ftf(u=u[i],a=0,m=mm1,b=bb1)
          quadBezier[i] = (1-w1)^2*0 + 2*(1-w1)*w1*mm1 + w1^2*(mm1+r*(bb1-mm1))
        }
        if( bb1<= u[i] & u[i]<= aa2){
          quadBezier[i] = mm1+r*(u[i]-mm1)
        }
        if( aa2<  u[i] & u[i]< bb2){
          w2  = ftf(u=u[i],a=aa2,m=mm2,b=bb2)
          quadBezier[i] = (1-w2)^2*(mm1+r*(aa2-mm1))+ 
            2*(1-w2)*w2*(cc + mm2/r) + 
            w2^2*(cc + bb2/r)
        }
        if( bb2<= u[i] & u[i]<= 1){
          quadBezier[i] = cc + u[i]/r
        }
      }
    }
    
    
    if (stage==0){
      r = (1+delta)/(1-delta)
      cc = 2*delta/(1+delta)
      m = (1-delta)/2
      a = 4/(5*r+4)
      b = (5-4*cc)*r/(5*r+4)
      quadBezier = array(,n)
      aa1 = (1-delta2)*3/4
      mm1 = (1-delta2)
      bb1 = (1-delta2) + (a+(1-a)*(1-delta2)-(1-delta2))/4
      aa2 = a+(1-a)*(1-delta2)
      mm2 = ((r-1)-mm1*r*(1-r))/(r^2-1)
      bb2 = b+(1-b)*(1-delta2)
      d = (mm1+mm2)/2
      for (i in 1:n){
        if( 0<=  u[i] & u[i]<  d){
          w1  = ftf(u=u[i],a=0,m=mm1,b=d)
          quadBezier[i] = 2*(1-w1)*w1*mm1 + w1^2*(mm1+r*(d-mm1))
        }
        if( d<=  u[i] & u[i]<= 1){
          w2  = ftf(u=u[i],a=d,m=mm2,b=1)
          quadBezier[i] = (1-w2)^2*(mm1+r*(d-mm1))+ 
            2*(1-w2)*w2*(cc + mm2/r) + 
            w2^2
        }
      }
    }	
    
  }
  
  
  if (delta==0|delta2==0){
    quadBezier = u
  }
  
  return(quadBezier)
}

invf = function(u,a,m,b,Ra,Rm,Rb){
  n = length(u)
  Ram  = Ra-Rm
  Ramb = Ra-2*Rm+Rb
  invf = array(,n)
  for (i in 1:n){
    cs = (Ram+sqrt(Ram^2-Ramb*(Ra-u[i])))/(Ramb)
    am = a-m
    amb = a-2*m+b
    invf[i] = ((cs*amb - am)^2-(m^2-a*b))/amb
  }
  return(invf)
}

InvQBezier = function(u,delta=0.1,delta2=1,stage=2){
  n = length(u)
  if (delta!=0){
    if (stage==2){
      r = (1+delta)/(1-delta)
      cc = 2*delta/(1+delta)
      m = (1-delta)/2
      a = 4/(5*r+4)
      b = (5-4*cc)*r/(5*r+4)
      InvQBezier = array(,n)
      aa1 = (1-delta2)*3/4
      mm1 = (1-delta2)
      bb1 = (1-delta2) + (a+(1-a)*(1-delta2)-(1-delta2))/4
      aa2 = a+(1-a)*(1-delta2)
      mm2 = ((r-1)-mm1*r*(1-r))/(r^2-1)
      bb2 = b+(1-b)*(1-delta2)
      for (i in 1:n){
        if( 0  <= u[i] & u[i]<= aa1){
          InvQBezier[i] = u[i]
        }
        if( aa1<  u[i] & u[i]<  mm1 + r*(bb1-mm1)){
          InvQBezier[i] = invf(u=u[i],a=aa1,m=mm1,b=bb1,Ra=aa1,Rm=mm1,Rb=mm1+r*(bb1-mm1))
        }
        if( mm1 + r*(bb1-mm1)<= u[i] & u[i]<= mm1+r*(aa2-mm1)){
          InvQBezier[i] = (u[i]-mm1+r*mm1)/r
        }
        if( mm1+r*(aa2-mm1)<  u[i] & u[i]< bb2/r+cc){
          InvQBezier[i] = invf(u=u[i],a=aa2,m=mm2,b=bb2,Ra=mm1+r*(aa2-mm1),Rm=cc + mm2/r,Rb=cc + bb2/r)
        }
        if( bb2/r+cc<= u[i] & u[i]<= 1){
          InvQBezier[i] = r*(u[i]-cc)
        }
        
      }
    }
    
    if (stage==1){
      r = (1+delta)/(1-delta)
      cc = 2*delta/(1+delta)
      m = (1-delta)/2
      a = 4/(5*r+4)
      b = (5-4*cc)*r/(5*r+4)
      InvQBezier = array(,n)
      aa1 = (1-delta2)*3/4
      mm1 = (1-delta2)
      bb1 = (1-delta2) + (a+(1-a)*(1-delta2)-(1-delta2))/4
      aa2 = a+(1-a)*(1-delta2)
      mm2 = ((r-1)-mm1*r*(1-r))/(r^2-1)
      bb2 = b+(1-b)*(1-delta2)
      for (i in 1:n){
        if( 0<=  u[i] & u[i]<  mm1 + r*(bb1-mm1)){
          InvQBezier[i] = invf(u=u[i],a=0,m=mm1,b=bb1,Ra=0,Rm=mm1,Rb=mm1+r*(bb1-mm1))
        }
        if( mm1 + r*(bb1-mm1)<= u[i] & u[i]<= mm1+r*(aa2-mm1)){
          InvQBezier[i] = (u[i]-mm1+r*mm1)/r
        }
        if( mm1+r*(aa2-mm1)<  u[i] & u[i]< bb2/r+cc){
          InvQBezier[i] = invf(u=u[i],a=aa2,m=mm2,b=bb2,Ra=mm1+r*(aa2-mm1),Rm=cc + mm2/r,Rb=cc + bb2/r)
        }
        if( bb2/r+cc<= u[i] & u[i]<= 1){
          InvQBezier[i] = r*(u[i]-cc)
        }
      }
    }
    if (stage==0){
      r = (1+delta)/(1-delta)
      cc = 2*delta/(1+delta)
      m = (1-delta)/2
      a = 4/(5*r+4)
      b = (5-4*cc)*r/(5*r+4)
      InvQBezier = array(,n)
      aa1 = (1-delta2)*7/8
      mm1 = (1-delta2)
      bb1 = (1-delta2) + (a+(1-a)*(1-delta2)-(1-delta2))/8
      aa2 = a+(1-a)*(1-delta2)
      mm2 = ((r-1)-mm1*r*(1-r))/(r^2-1)
      bb2 = b+(1-b)*(1-delta2)
      d = (mm1+mm2)/2
      for (i in 1:n){
        
        if( 0<=  u[i] & u[i]<  mm1 + r*(d-mm1)){
          InvQBezier[i] = invf(u=u[i],a=0,m=mm1,b=d,Ra=0,Rm=mm1,Rb=mm1+r*(d-mm1))
        }
        if( mm1+r*(d-mm1)<=  u[i] & u[i]<= 1){
          InvQBezier[i] = invf(u=u[i],a=d,m=mm2,b=1,Ra=mm1+r*(d-mm1),Rm=cc + mm2/r,Rb=1)
        }
      }
    }
    
  }
  
  if (delta==0|delta2==0){
    InvQBezier = u
  }
  return(InvQBezier)
}

rqB = function(n = 10, delta=0, delta2=1, stage=1){
  rqB = array(,n)
  rqB = InvQBezier(runif(n),delta=delta,delta2=delta2,stage=stage)
  return(rqB)
}



###Local power alternatives

subcos = function(u=u,a=0.5){
  n = length(u)
  subcos=array(,n)
  for(i in 1:n){
    if(u[i]<a){
      subcos[i] = 0
    }
    if(u[i]>=a){
      subcos[i] = ((cos((u[i]-a)/(1-a)*2*pi) - 1))#^(2)#/(u[i]+2)
    }
  }
  return(subcos)
}

ArqB = function(n=100,delta=0.4,delta2=0.95,sc=1/35){
  d1 = delta
  d2 = delta2
  r = (1+d1)/(1-d1)
  cc = 2*d1/(1+d1)
  m = (1-d1)/2
  a = 4/(5*r+4)
  b = (5-4*cc)*r/(5*r+4)
  aa1 = (1-d2)*3/4
  mm1 = (1-d2)
  bb1 = (1-d2) + (a+(1-a)*(1-d2)-(1-d2))/4
  aa2 = a+(1-a)*(1-d2)
  mm2 = ((r-1)-mm1*r*(1-r))/(r^2-1)
  bb2 = b+(1-b)*(1-d2)
  #sc = 1/35
  ArqB = array(,n)
  s = 0
  while(s < n){
    V = rqB(n=1,delta=d1,delta2=d2,stage=1)
    if (V<=bb2){
      s = s + 1
      ArqB[s] = V
    }
    if (V>bb2){
      ss = 0
      VV = V
      U = runif(1)
      while(ss<1){
        if(U < (1-sin(((VV-bb2)*2*pi)/(1-bb2))*2*pi/(1-bb2)*sc*r)/2){
          ArqB[s] = VV
          ss = ss+1
        }
        else{
          U = runif(1)
          sss = 0
          while(sss==0){
            VV = rqB(n=1,delta=d1,delta2=d2,stage=1)
            if(VV>bb2){
              sss = sss+1
            }
          }
          
        }
      }
    }
  }
  return(ArqB)
}

RL = function(u,delta=0){
  n = length(u)
  RL = array(,n)
  for (i in 1:n){
    if( u[i]>=0 & u[i]<=delta){
      RL[i] = u[i]
    }
    if( u[i]>delta & u[i]<=1/18+delta){
      RL[i] = delta + (u[i]-delta)*8
    }
    if( u[i]>1/18+delta & u[i]<=1/2+delta){
      RL[i] = (8/18+delta) + (u[i] - (1/18 + delta))/8
    }
    if( u[i]>1/2+delta & u[i]<=1){
      RL[i] = u[i]
    }
    
  }
  return(RL)
}

RI = function(u,delta=0){
  n = length(u)
  RI = array(,n)
  for (i in 1:n){
    if( u[i]>=0 & u[i]<=delta){
      RI[i] = u[i]
    }
    if( u[i]>delta & u[i]<=8/18+delta){
      RI[i] = delta + (u[i]-delta)/8
    }
    if( u[i]>8/18+delta & u[i]<=1/2+delta){
      RI[i] = (1/18+delta) + (u[i] - (8/18 + delta))*8
    }
    if( u[i]>1/2+delta & u[i]<=1){
      RI[i] = u[i]
    }
    
  }
  return(RI)
}

#######Smoothed RL and RI
RLS = function(u,delta=0){
  n = length(u)
  RLS = array(,n)
  a1 = 7/8*delta
  m1 = delta
  b1 = delta + 1/(8*18)
  a2 = delta + 7/(8*18)
  m2 = delta + 1/18
  b2 = delta + 1/9
  a3 = delta + 4/9
  m3 = delta + 1/2
  b3 = 7/8*delta + 9/16
  Ra1 = 7/8*delta
  Rm1 = delta
  Rb1 = delta + 1/18
  Ra2 = delta + 7/18
  Rm2 = delta + 8/18
  Rb2 = delta + (65)/(18*8)
  Ra3 = delta + (71)/(18*8)
  Rm3 = delta + 1/2
  Rb3 = 7/8*delta + 9/16
  for (i in 1:n){
    if( u[i]>=0 & u[i]<=a1){
      RLS[i] = u[i]
    }
    if( u[i]>a1 & u[i]<=b1){
      w = ftf(u=u[i],a=a1,m=m1,b=b1)
      RLS[i] = (1-w)^2*Ra1 + 2*(1-w)*w*Rm1 + w^2*Rb1
    }
    if( u[i]>b1 & u[i]<=a2){
      RLS[i] = delta + (u[i]-delta)*8
    }
    if( u[i]>a2 & u[i]<=b2){
      w = ftf(u=u[i],a=a2,m=m2,b=b2)
      RLS[i] = (1-w)^2*Ra2 + 2*(1-w)*w*Rm2 + w^2*Rb2
    }
    if( u[i]>b2 & u[i]<=a3){
      RLS[i] = (8/18+delta) + (u[i] - (1/18+delta))/8
    }
    if( u[i]>a3 & u[i]<=b3){
      w = ftf(u=u[i],a=a3,m=m3,b=b3)
      RLS[i] = (1-w)^2*Ra3 + 2*(1-w)*w*Rm3 + w^2*Rb3
    }
    if( u[i]>b3 & u[i]<=1){
      RLS[i] = u[i]
    }
    
  }
  return(RLS)
}

RIS = function(u,delta=0){
  n = length(u)
  RIS = array(,n)
  Ra1 = 7/8*delta
  Rm1 = delta
  Rb1 = delta + 1/(8*18)
  Ra2 = delta + 7/(8*18)
  Rm2 = delta + 1/18
  Rb2 = delta + 1/9
  Ra3 = delta + 4/9
  Rm3 = delta + 1/2
  Rb3 = 7/8*delta + 9/16
  a1 = 7/8*delta
  m1 = delta
  b1 = delta + 1/18
  a2 = delta + 7/18
  m2 = delta + 8/18
  b2 = delta + (65)/(18*8)
  a3 = delta + (71)/(18*8)
  m3 = delta + 1/2
  b3 = 7/8*delta + 9/16
  for (i in 1:n){
    if( u[i]>=0 & u[i]<=a1){
      RIS[i] = u[i]
    }
    if( u[i]>a1 & u[i]<=b1){
      RIS[i] = invf(u=u[i],a=Ra1,m=Rm1,b=Rb1,Ra=a1,Rm=m1,Rb=b1)
    }
    if( u[i]>b1 & u[i]<=a2){
      RIS[i] = delta + (u[i]-delta)/8
    }
    if( u[i]>a2 & u[i]<=b2){
      RIS[i] = invf(u=u[i],a=Ra2,m=Rm2,b=Rb2,Ra=a2,Rm=m2,Rb=b2)
    }
    if( u[i]>b2 & u[i]<=a3){
      RIS[i] = (1/18+delta) + (u[i] - (8/18 + delta))*8
    }
    if( u[i]>a3 & u[i]<=b3){
      RIS[i] = invf(u=u[i],a=Ra3,m=Rm3,b=Rb3,Ra=a3,Rm=m3,Rb=b3)
    }
    if( u[i]>b3 & u[i]<=1){
      RIS[i] = u[i]
    }
    
  }
  
  return(RIS)
}


######################Strictly


STquadBezier = function(u,delta=0.1,delta2=1){
  n = length(u)
  if (delta!=0){
    r = (1+delta)/(1-delta)
    cc = 2*delta/(1+delta)
    m = (1-delta)/2
    a = 4/(5*r+4)
    b = (5-4*cc)*r/(5*r+4)
    STquadBezier = array(,n)
    aa1 = (1-delta2)*3/4
    mm1 = (1-delta2)
    bb1 = (1-delta2) + (a+(1-a)*(1-delta2)-(1-delta2))/4
    aa2 = a+(1-a)*(1-delta2)
    mm2 = ((r-1)-mm1*r*(1-r))/(r^2-1)
    bb2 = b+(1-b)*(1-delta2)
    d = (mm1+mm2)/2
    for (i in 1:n){
      if( 0<=  u[i] & u[i]<  d){
        w1  = ftf(u=u[i],a=0,m=mm1,b=d)
        STquadBezier[i] = 2*(1-w1)*w1*mm1 + w1^2*(mm1+r*(d-mm1))
      }
      if( d<=  u[i] & u[i]<= 1){
        w2  = ftf(u=u[i],a=d,m=mm2,b=1)
        STquadBezier[i] = (1-w2)^2*(mm1+r*(d-mm1))+ 
          2*(1-w2)*w2*(cc + mm2/r) + 
          w2^2
      }
    }
  }
  if (delta==0|delta2==0){
    STquadBezier = u
  }
  
  return(STquadBezier)
}

STInvQBezier = function(u,delta=0.1,delta2=1){
  n = length(u)
  if (delta!=0){
    r = (1+delta)/(1-delta)
    cc = 2*delta/(1+delta)
    m = (1-delta)/2
    a = 4/(5*r+4)
    b = (5-4*cc)*r/(5*r+4)
    STInvQBezier = array(,n)
    aa1 = (1-delta2)*7/8
    mm1 = (1-delta2)
    bb1 = (1-delta2) + (a+(1-a)*(1-delta2)-(1-delta2))/8
    aa2 = a+(1-a)*(1-delta2)
    mm2 = ((r-1)-mm1*r*(1-r))/(r^2-1)
    bb2 = b+(1-b)*(1-delta2)
    d = (mm1+mm2)/2
    for (i in 1:n){
      
      if( 0<=  u[i] & u[i]<  mm1 + r*(d-mm1)){
        STInvQBezier[i] = invf(u=u[i],a=0,m=mm1,b=d,Ra=0,Rm=mm1,Rb=mm1+r*(d-mm1))
      }
      if( mm1+r*(d-mm1)<=  u[i] & u[i]<= 1){
        STInvQBezier[i] = invf(u=u[i],a=d,m=mm2,b=1,Ra=mm1+r*(d-mm1),Rm=cc + mm2/r,Rb=1)
      }
    }
  }
  if (delta==0|delta2==0){
    STInvQBezier = u
  }
  
  return(STInvQBezier)
}

rSTqB = function(n = 10, delta=0, delta2=1){
  rSTqB = array(,n)
  rSTqB = STInvQBezier(runif(n),delta=delta,delta2=delta2)
  return(rSTqB)
}



RotaLi = function(u,angle,radius=0.4){
  a = 1/2-cos(angle)*radius; b = 1/2+cos(angle)*radius;
  Ind = c(a<=u & u<=b); 
  RotaLi = (1/2 + (u-1/2)*tan(angle))*Ind
  return(RotaLi); 
}
RotaInvLi = function(v,angle,radius=0.4){
  a = 1/2-cos(angle)*radius; b = 1/2+cos(angle)*radius;
  La = 1/2-sin(angle)*radius; Lb = 1/2+sin(angle)*radius; 
  Ind = c(La<=v & v<=Lb); 
  RotaInvLi = (1/2 + (v-1/2)/tan(angle))*Ind
  return(RotaInvLi); 
}
ConnLi0 = function(u,angle,radius=0.4){
  La = 1/2 - sin(angle)*radius; a = 1/2-cos(angle)*radius;
  Ind = c(0<=u & u<=a); 
  ConnLi0 = (0 + (u-0)*(La/a) )*Ind
  return(ConnLi0); 
}
ConnLi1 = function(u,angle,radius=0.4){
  Lb = 1/2 + sin(angle)*radius; b = 1/2+cos(angle)*radius;
  Ind = c(b<=u & u<=1); 
  ConnLi1 = (1 - (1-u)*((1-Lb)/(1-b)) )*Ind
  return(ConnLi1); 
}
InvRotaLi = function(v,angle,radius=0.4){
  La = 1/2 - sin(angle)*radius; Lb = 1/2 + sin(angle)*radius;
  a = 1/2 - cos(angle)*radius ;  b = 1/2 + cos(angle)*radius;
  Ind = c(La<=v&v<=Lb); 
  InvRotaLi = (1/2 + (v-1/2)/tan(angle))*Ind
  return(InvRotaLi); 
}
InvConnLi0 = function(v,angle,radius=0.4){
  La = 1/2 - sin(angle)*radius; 
  a = 1/2 - cos(angle)*radius ; 
  Ind = c(0<=v&v<=La); 
  InvConnLi0 = (0 + (v-0)/(La/a))*Ind
  return(InvConnLi0); 
}
InvConnLi1 = function(v,angle,radius=0.4){
  Lb = 1/2 + sin(angle)*radius;
  b = 1/2 + cos(angle)*radius;
  Ind = c(Lb<=v&v<=1); 
  InvConnLi1 = (1 - (1-v)/((1-Lb)/(1-b)))*Ind
  return(InvConnLi1); 
}
RotaGlueQB0 = function(u,angle,radius=0.4,d=1/3){
  a2  = 1/2-cos(angle)*radius; 
  mab = 1/2-cos(angle)*(radius+0.05); 
  b1  = mab*(1-d); 
  ya2  = 1/2-sin(angle)*radius
  ymab = 1/2-sin(angle)*(radius+0.05)
  yb1 = ymab*(1-d)
  RotaGlueQB0 = QB(u,c(b1,mab,a2),c(yb1,ymab,ya2))*c(b1<=u&u<a2);
  return(RotaGlueQB0)
}
RotaInvGlueQB0 = function(v,angle,radius=0.4,d=1/3){
  a2  = 1/2-cos(angle)*radius; 
  mab = 1/2-cos(angle)*(radius+0.05); 
  b1  = mab*(1-d); 
  ya2  = 1/2-sin(angle)*radius
  ymab = 1/2-sin(angle)*(radius+0.05)
  yb1 = ymab*(1-d)
  RotaInvGlueQB0 = InvQB(v,c(b1,mab,a2),c(yb1,ymab,ya2))*c(yb1<=v&v<ya2);
  return(RotaInvGlueQB0)
}
RotaGlueQB1 = function(u,angle,radius=0.4,d=1/3){
  b1  = 1/2 + cos(angle)*radius; 
  mab = 1/2 + cos(angle)*(radius+0.05); 
  a2  = 1 - (1-mab)*(1-d); 
  yb1  = 1/2 + sin(angle)*radius; 
  ymab = 1/2 + sin(angle)*(radius+0.05); 
  ya2  = 1-(1-ymab)*(1-d); 
  RotaGlueQB1 = QB(u,c(b1,mab,a2),c(yb1,ymab,ya2))*c(b1<u&u<=a2);
  return(RotaGlueQB1)
}
RotaInvGlueQB1 = function(v,angle,radius=0.4,d=1/3){
  b1  = 1/2 + cos(angle)*radius; 
  mab = 1/2 + cos(angle)*(radius+0.05); 
  a2  = 1 - (1-mab)*(1-d); 
  yb1  = 1/2 + sin(angle)*radius; 
  ymab = 1/2 + sin(angle)*(radius+0.05); 
  ya2  = 1-(1-ymab)*(1-d); 
  RotaInvGlueQB1 = InvQB(v,c(b1,mab,a2),c(yb1,ymab,ya2))*c(yb1<v&v<=ya2);
  return(RotaInvGlueQB1)
}
RotaConn0 = function(u,angle,radius=0.4,d=1/3){
  mab = 1/2-cos(angle)*(radius+0.05); 
  b1  = mab*(1-d); 
  ymab = 1/2-sin(angle)*(radius+0.05)
  yb1 = ymab*(1-d)
  Ind = c(0<=u & u<b1); 
  RotaConn0 = (0 + (u-0)*(yb1/b1) )*Ind
  return(RotaConn0)
}
RotaInvConn0 = function(v,angle,radius=0.4,d=1/3){
  mab = 1/2-cos(angle)*(radius+0.05); 
  b1  = mab*(1-d); 
  ymab = 1/2-sin(angle)*(radius+0.05)
  yb1 = ymab*(1-d)
  Ind = c(0<=v & v<yb1); 
  RotaInvConn0 = (0 + (v-0)*(b1/yb1) )*Ind
  return(RotaInvConn0)
}
RotaConn1 = function(u,angle,radius=0.4,d=1/3){
  mab = 1/2 + cos(angle)*(radius+0.05); 
  a2  = 1 - (1-mab)*(1-d); 
  ymab = 1/2 + sin(angle)*(radius+0.05); 
  ya2  = 1-(1-ymab)*(1-d); 
  Ind = c(a2<u & u<=1); 
  RotaConn1 = (1 - (1-u)*((1-ya2)/(1-a2)) )*Ind
  return(RotaConn1)
}
RotaInvConn1 = function(v,angle,radius=0.4,d=1/3){
  mab = 1/2 + cos(angle)*(radius+0.05); 
  a2  = 1 - (1-mab)*(1-d); 
  ymab = 1/2 + sin(angle)*(radius+0.05); 
  ya2  = 1-(1-ymab)*(1-d); 
  Ind = c(ya2<v & v<=1); 
  RotaInvConn1 = (1 - (1-v)*((1-a2)/(1-ya2)) )*Ind
  return(RotaInvConn1)
}
pRotaODC = function(u,angle,radius=0.4,d=1/3){
  pRotaODC = (
    RotaConn0(u,angle,radius,d)
    +RotaGlueQB0(u,angle,radius,d)
    +RotaLi(u,angle,radius)
    +RotaGlueQB1(u,angle,radius,d)
    +RotaConn1(u,angle,radius,d)
  )
  return(pRotaODC)
}
qRotaODC = function(v,angle,radius=0.4,d=1/3){
  qRotaODC = (
    RotaInvConn0(v,angle,radius,d)
    +RotaInvGlueQB0(v,angle,radius,d)
    +RotaInvLi(v,angle,radius)
    +RotaInvGlueQB1(v,angle,radius,d)
    +RotaInvConn1(v,angle,radius,d)
  )
  return(qRotaODC)
}
rRotaODC = function(n=150,angle,radius=0.4,d=1/3){
  rRotaODC = qRotaODC(runif(n),angle,radius,d)
  return(rRotaODC)
}


tu = function(u,xv3){
  x1 = xv3[1]; x2 = xv3[2]; x3 = xv3[3]; 
  tu = array(,length(u))
  if(2*x2 == x1+x3){print("Warning! Bezier curve is flat!")}
  if(2*x2 != x1+x3){
    tu = ( x1 - x2 + 
             sqrt( abs(x2^2 - x1*x3 + (x1 - 2*x2 + x3)*u) )
    ) / (x1 - 2*x2 + x3) * 
      c(x1 <= u & u <= x3)
  }
  return(tu)
}

QB = function(u,xv3,yv3){
  x1 = xv3[1]; x2 = xv3[2]; x3 = xv3[3]; 
  y1 = yv3[1]; y2 = yv3[2]; y3 = yv3[3]; 
  w = tu(u,xv3); 
  QB = ((1-w)^2*y1 + 2*(1-w)*w*y2 + w^2*y3)*c(x1<=u&u<=x3); 
  return(QB)
}

InvQB = function(v,xv3,yv3){
  x1 = xv3[1]; x2 = xv3[2]; x3 = xv3[3]; 
  y1 = yv3[1]; y2 = yv3[2]; y3 = yv3[3]; 
  w = tu(v,yv3); 
  InvQB = ((1-w)^2*x1 + 2*(1-w)*w*x2 + w^2*x3)*c(y1<=v&v<=y3); 
  return(InvQB)
}

GlueQB = function(u,b1,a2,s1,s2,d=0.01){
  distance = a2-b1; 
  yb1 = 1 - (1-b1)*s1; ya2 = 1- (1-a2)*s2; 
  b1r = b1 + distance*d; yb1r = 1 - (1-b1r)*s1; 
  a2l = a2 - distance*d; ya2l = 1 - (1-a2l)*s2; 
  mab = (b1r+a2l)/2; ymab = (yb1r+ya2l)/2; 
  QB1 = QB(u,c(b1,b1r,mab),c(yb1,yb1r,ymab));
  QB2 = QB(u,c(mab,a2l,a2),c(ymab,ya2l,ya2));
  GlueQB = QB1 + QB2*c(u>mab); 
  return(GlueQB)
}

InvGlueQB = function(v,b1,a2,s1,s2,d=0.01){
  distance = a2-b1; 
  yb1 = 1 - (1-b1)*s1; ya2 = 1- (1-a2)*s2; 
  b1r = b1 + distance*d; yb1r = 1 - (1-b1r)*s1; 
  a2l = a2 - distance*d; ya2l = 1 - (1-a2l)*s2; 
  mab = (b1r+a2l)/2; ymab = (yb1r+ya2l)/2; 
  InvQB1 = InvQB(v,c(b1,b1r,mab),c(yb1,yb1r,ymab));
  InvQB2 = InvQB(v,c(mab,a2l,a2),c(ymab,ya2l,ya2));
  InvGlueQB = InvQB1 + InvQB2*c(v>ymab); 
  return(InvGlueQB)
}

STLi0 = function(u,a=0.3,b=0.7,slope=4,d=0.01){
  Ind = c( 0<=u & u < a-d ); 
  STLi0 = u * Ind; 
  return(STLi0)
}
STInvLi0 = function(v,a=0.3,b=0.7,slope=4,d=0.01){
  Ind = c( 0<=v & v < a-d ); 
  STInvLi0 = v * Ind; 
  return(STInvLi0)
}
STGlueQB01 = function(u,a=0.3,b=0.7,slope=4,d=0.01){
  b1 = a-d; mab = a; a2 = a + d/2; 
  yb1 = a-d; ymab = a; ya2 = a + d/2*slope; 
  Ind = c(b1 <=u & u < a2); 
  STGlueQB01 = QB(u,c(b1,mab,a2),c(yb1,ymab,ya2))*Ind; 
  return(STGlueQB01)
}
STInvGlueQB01 = function(v,a=0.3,b=0.7,slope=4,d=0.01){
  b1 = a-d; mab = a; a2 = a + d/2; 
  yb1 = a-d; ymab = a; ya2 = a + d/2*slope; 
  Ind = c(yb1 <=v & v < ya2); 
  STInvGlueQB01 = InvQB(v,c(b1,mab,a2),c(yb1,ymab,ya2))*Ind; 
  return(STInvGlueQB01)
}
STLi1 = function(u,a=0.3,b=0.7,slope=4,d=0.01){
  Ind = c( a + d/2 <=u 
           & u < a + (b-a)* ( 1 / (slope+1) ) - d/2); 
  STLi1 = (a + slope*(u-a))* Ind; 
  return(STLi1)
}
STInvLi1 = function(v,a=0.3,b=0.7,slope=4,d=0.01){
  a1 = a + d/2; b1 = a + (b-a)* ( 1 / (slope+1) ) - d/2; 
  ya1 = a + slope*d/2; yb1 = a + slope*((b-a)* ( 1 / (slope+1) ) - d/2)
  Ind = c( ya1 <=v & v < yb1); 
  STInvLi1 = (a1 + slope^(-1)*(v-ya1) )* Ind; 
  return(STInvLi1)
}
STGlueQB12 = function(u,a=0.3,b=0.7,slope=4,d=0.01){
  b1 = a + (b-a)* ( 1 / (slope+1) ) - d/2; 
  mab = a + (b-a)* ( 1 / (slope+1) ); 
  a2 = a + (b-a)* ( 1 / (slope+1) ) + d; 
  yb1 = a + slope*( (b-a)* ( 1 / (slope+1) ) - d/2)
  ymab = a + slope* (b-a)*( 1 / (slope+1) ) 
  ya2 = b - ( (b-a)*(slope/(slope+1)) - d)*slope^(-1)
  Ind = c(b1<=u & u < a2); 
  STGlueQB12 = QB(u,c(b1,mab,a2),c(yb1,ymab,ya2))*Ind; 
  return(STGlueQB12)
}
STInvGlueQB12 = function(v,a=0.3,b=0.7,slope=4,d=0.01){
  b1 = a + (b-a)* ( 1 / (slope+1) ) - d/2; 
  mab = a + (b-a)* ( 1 / (slope+1) ); 
  a2 = a + (b-a)* ( 1 / (slope+1) ) + d; 
  
  yb1 = a + slope*( (b-a)* ( 1 / (slope+1) ) - d/2)
  ymab = a + slope* (b-a)*( 1 / (slope+1) ) 
  ya2 = b - ( (b-a)*(slope/(slope+1)) - d)*slope^(-1)
  
  Ind = c(yb1<=v & v < ya2); 
  STInvGlueQB12 = InvQB(v,c(b1,mab,a2),c(yb1,ymab,ya2))*Ind; 
  return(STInvGlueQB12)
}
STLi2 = function(u,a=0.3,b=0.7,slope=4,d=0.01){
  Ind = c( a + (b-a)* ( 1 / (slope+1) ) + d <=u 
           & u < b - d); 
  STLi2 = (b + slope^(-1)*(u-b))* Ind; 
  return(STLi2)
}
STInvLi2 = function(v,a=0.3,b=0.7,slope=4,d=0.01){
  a1 = a + (b-a)* ( 1 / (slope+1) ) + d ; 
  b1 = b - d;
  ya1 = b - slope^(-1) * (b-a1); 
  yb1 = b - slope^(-1) * (b-b1); 
  Ind = c( ya1 <=v & v < yb1); 
  STInvLi2 = (b1 + slope*(v-yb1) )* Ind; 
  return(STInvLi2)
}
STGlueQB23 = function(u,a=0.3,b=0.7,slope=4,d=0.01){
  b1 = b-d; 
  mab = b; 
  a2 = b + d/2; 
  yb1 = b - slope^(-1)*d; 
  ymab = b ; 
  ya2 = b + d/2; 
  Ind = c(b-d<=u & u < b+d/2); 
  STGlueQB23 = QB(u,c(b1,mab,a2),c(yb1,ymab,ya2))*Ind; 
  return(STGlueQB23)
}
STInvGlueQB23 = function(v,a=0.3,b=0.7,slope=4,d=0.01){
  b1 = b-d; 
  mab = b; 
  a2 = b + d/2; 
  yb1 = b - slope^(-1)*d; 
  ymab = b ; 
  ya2 = b + d/2; 
  Ind = c(yb1<=v & v < ya2); 
  STInvGlueQB23 = InvQB(v,c(b1,mab,a2),c(yb1,ymab,ya2))*Ind; 
  return(STInvGlueQB23)
}
STLi3 = function(u,a=0.3,b=0.7,slope=4,d=0.01){
  Ind = c( b + d / 2 <=u & u <= 1); 
  STLi4 = u * Ind; 
  return(STLi4)
}
STInvLi3 = function(u,a=0.3,b=0.7,slope=4,d=0.01){
  Ind = c( b + d / 2 <=u & u <= 1); 
  STLi4 = u * Ind; 
  return(STLi4)
}
pSTODC = function(u,a=0.3,b=0.7,slope=4,d=0.01){
  if(b==a){pSTODC = u; }
  if(a<b){
    pSTODC = (STLi0(u,a,b,slope,d) 
              +STGlueQB01(u,a,b,slope,d)
              +STLi1(u,a,b,slope,d)
              +STGlueQB12(u,a,b,slope,d)
              +STLi2(u,a,b,slope,d)
              +STGlueQB23(u,a,b,slope,d)
              +STLi3(u,a,b,slope,d)
    )
  }
  return(pSTODC)
}
qSTODC = function(u,a=0.3,b=0.7,slope=4,d=0.01){
  if(a==b){qSTODC = v; }
  if(a<b){
    qSTODC = (STInvLi0(u,a,b,slope) + 
                STInvGlueQB01(u,a,b,slope) + 
                STInvLi1(u,a,b,slope) + 
                STInvGlueQB12(u,a,b,slope) + 
                STInvLi2(u,a,b,slope) + 
                STInvGlueQB23(u,a,b,slope) + 
                STInvLi3(u,a,b,slope)
    )
  }
  return(qSTODC)
}
rSTODC = function(n=150,a=0.3,b=0.7,slope=4,d=0.01){
  if(a==b){rSTODC = runif(n)}; 
  if(a<b){rSTODC = qSTODC(runif(n),a,b,slope,d); }
  return(rSTODC)
}
