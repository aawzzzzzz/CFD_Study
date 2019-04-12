!Lax-Wendroff scheme
program main
  implicit none
  real,dimension(-21:41,0:10000) ::u
  real,dimension(-21:41) ::x
  real:: dt=0.05,dx=0.1,a=1.0,t=2
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
  u(41,:)=0.0

  do j=0,N-1
    do i=-20,40
        u(i,j+1)=u(i,j)-0.5*a*dt/dx*(u(i+1,j)-u(i-1,j))+0.5*((a*dt/dx)**2)*(u(i+1,j)-2*u(i,j)+u(i-1,j))
    end do
  end do

  do i=-20,40
    write(*,*) u(i,N)
    write(10,*) x(i),u(i,N)
  end do


  write(*,*) "compute completed!"
end
