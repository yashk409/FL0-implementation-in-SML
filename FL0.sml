datatype FLV = X
            |  Z
            |  T
            |  F
            |  S of FLV
            |  P of FLV
            |  IZ of FLV
            |  GTZ of FLV
            |  ITEB of FLV * FLV * FLV
            |  ITE of FLV * FLV * FLV
            |  Abst of FLV
            |  Appl of FLV * FLV
            |  SYNTAX_ERROR

fun betared(X,y) = y  
   |betared(S(x),y)=S(betared(x,y))
   |betared(P(x),y)=P(betared(x,y))
   |betared(IZ(x),y)=IZ(betared(x,y))
   |betared(GTZ(x),y)=GTZ(betared(x,y))
   |betared(ITEB(x,alpha,beta),y)=ITEB(betared(x,y),betared(alpha,y),betared(beta,y))
   |betared(ITE(x,alpha,beta),y)=ITE(betared(x,y),betared(alpha,y),betared(beta,y))    
   |betared(Appl(x,z),y)=betared((betared(x,y)),z)                    
   |betared(x,y)=x

fun reduce Z = Z
   |reduce T =T
   |reduce F = F
   |reduce (S(P(x))) = if (x=T orelse x=F) 
                    then SYNTAX_ERROR 
                    else reduce(x)

   |reduce (P(S(x))) = if (x=T orelse x=F) 
                    then SYNTAX_ERROR 
                    else reduce(x)

   |reduce (S(x))=if (reduce(x)=T orelse reduce(x)=F) 
                  then SYNTAX_ERROR
                  else if (reduce(x)=x) 
                  then S(x)
                  else reduce(S(reduce(x)))

   |reduce (P(x))=if (reduce(x)=T orelse reduce(x)=F) 
                  then SYNTAX_ERROR
                  else if (reduce(x)=x) 
                  then P(x)
                  else reduce(P(reduce(x)))

   |reduce(ITEB(x,y,z))=
       if (not(reduce(y)=T orelse reduce(y)=F)) orelse (not(reduce(z)=T orelse reduce(z)=F)) 
       then SYNTAX_ERROR
       else if reduce(x)=T 
       then reduce(y)
       else if reduce(x)=F 
       then reduce(z)
       else SYNTAX_ERROR
    
    |reduce(ITE(x,y,z))=
       if (reduce(y)=T orelse reduce(y)=F) orelse (reduce(z)=T orelse reduce(z)=F) 
       then SYNTAX_ERROR
       else if reduce(x)=T 
       then reduce(y)
       else if reduce(x)=F 
       then reduce(z)
       else SYNTAX_ERROR

    |reduce (IZ(x))=
       if(reduce(x)=T orelse reduce(x)=F) 
       then SYNTAX_ERROR
       else if(reduce(x)=Z)
       then T
       else F

    |reduce(GTZ(x))=let
                    fun checkgtz(S(x))=T
                       |checkgtz(P(x))=F
                       |checkgtz(Z)=F
                       |checkgtz(x)=SYNTAX_ERROR
                    in
                      checkgtz(reduce(x))
                    end
    |reduce(Appl(Abst(x),y))= reduce(betared(x,y))
    |reduce(Appl(x,y))= reduce(x)
    |reduce(Abst(x))=Abst(x)       
    |reduce(X)=X
    |reduce(x)= SYNTAX_ERROR
