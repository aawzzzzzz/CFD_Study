program main
  implicit none
  real, dimension(0:20000,0:20000)::theta
  integer :: i,j,N=2
  real:: dtao=0.005, g=1.0,Bi=1.0,dts
  dts=dtao*(N**2)
  theta(:,0)=1.0
  do j=0,9999
    do i=0,N
        if(i==0)then
            theta(i,j+1)=(1-2*dts)*theta(i,j)+2*dts*theta(i+1,j)+g*dtao
        else if((i>0.).AND.(i<N))then
            theta(i,j+1)=(1-2*dts)*theta(i,j)+dts*(theta(i+1,j)+theta(i-1,j))+g*dtao
        else if(i==N)then
            theta(i,j+1)=(1-2*dts-2*Bi/N*dts)*theta(i,j)+2*dts*theta(i-1,j)+g*dtao
        end if
    end do
  end do

  do i=0,N
    write(*,*) theta(i,10000)
  end do

end
