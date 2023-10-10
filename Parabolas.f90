program EjercicioParabolasSimpson			!Diego Villegas Paula Irán

!Encontrar la densidad lineal de masa usando método de parábolas, donde lambda = cx, mTotal= 3k y L=2m 

!Definimos nuestras variabes 
real :: a, b, h, c, CentroMasa
integer :: n, i, j

!Inicializamos nuestras variables. 
dimension x(0:1000), fx(0:1000), xlambda(0:1000) 

n=200; a=0.; b=2.; h=(b-a)/n; masaTotal=3.

do i=0, n
  x(i) = a+i*h
  fx(i)=fun(x(i))  
end do

nsub=n/2

do j=1, nsub
   x0=x(2*j-2); x1=x(2*j-1) ; x2=x(2*j)  
   y0=fun(x0); y1=fun(x1); y2=fun(x2) 
   suma = suma + h/3.*(y0+4*y1+y2) 
end do   
!print *, suma

c=masaTotal/suma

write(*,*) 'El coeficiente de la densidad lineal de masa es: ', c 

!Ahora obtengamos el centro de masa con el resultado anterior 

do k=0, n
  xlambda(i)=c*x(i)
  fx(i)=fun(x(i))  
end do

do j=1, nsub
   x0=x(2*(j-1)); x1=x(2*(j-1)+1) ; x2=x(2*(j-1)+2)  
   y0=fun(x0)*x0*c; y1=fun(x1)*x1*c; y2=fun(x2)*x2*c 
   CentroMasa = CentroMasa + h/3.*(y0+4*y1+y2) 
end do   

write(*,*) 'El centro de masa es: ', CentroMasa/masaTotal

end

!Definimos la función con la que queremos trabajar 
function fun(z) 
fun=z
end 

