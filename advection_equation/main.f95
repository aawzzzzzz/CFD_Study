!minmod limiter
program main
  implicit none
  real::minmod
  real,dimension(-22:42,0:10000) ::u
  real,dimension(-22:42) ::x
  real:: dt=0.05,dx=0.1,a=1.0,t=2,sigma1,sigma2,s1,s2,s3,s4
  integer i,j,N

  N=int(t/dt)

  do i=-21,41
    x(i)=real(i)*dx
  end do

  do i=-21,41
    if (x(i)<0)then
        u(i,0)=1.0
    else
        u(i,0)=0
    end if
  end do

  u(-21,:)=1.0
  u(-22,:)=1.0
  u(41,:)=0.0
  u(42,:)=0.0

  do j=0,N-1
    do i=-20,40
        s1=(u(i,j)-u(i-1,j))/dx
        s2=(u(i-1,j)-u(i-2,j))/dx
        s3=(u(i+1,j)-u(i,j))/dx
        s4=(u(i,j)-u(i-1,j))/dx
        sigma1=minmod(s1,s2)
        sigma2=minmod(s3,s4)
!        write(*,*)s1,s2,s3,s4
!        write(*,*)sigma2,sigma1
        u(i,j+1)=u(i,j)-a*dt/dx*(u(i,j)-u(i-1,j))-&
        0.5*(a*dt/dx)*(dx-a*dt)*(sigma2-sigma1)
    end do
  end do

  do i=-20,40
    write(*,*) u(i,N)
    write(10,*) x(i),u(i,N)
  end do


  write(*,*) "compute completed!"
end

real function minmod(a1,a2)
  implicit none
  real,intent(in)::a1
  real,intent(in)::a2
  if(a1*a2>0)then
    minmod=a1/abs(a1)*min(abs(a1),abs(a2))
  else
    minmod=0
  end if
end function minmod
