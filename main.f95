program main
  implicit none
  real, dimension(10)::aa,bb,cc,theta
  integer :: i,j,N=8,M
  real:: dtao=0.05, g=1.0,Bi=1.0,dts,gtao,dts1,tao=1

  dts=dtao*(N**2)
  theta=1.0
  dts1=1+2*dts
  gtao=g*dtao
  aa=0
  bb=0
  cc=0
  M=int(tao/dtao)

!  do j=2,2+N,1
!    if (j==2)then
!       aa(j)=0.0
!       bb(j)=dts1
!       cc(j)=-2*dts
!    else if((j<2+N).AND.(j>2))then
!       aa(j)=-dts
!       bb(j)=dts1
!       cc(j)=-dts
!    else if(j==2+N)then
!       aa(j)=-2*dts
!       bb(j)=dts1+2*Bi/N*dts
!       cc(j)=0.0
!    end if
!  end do


  do i=1,M
    do j=2,2+N,1
      if (j==2)then
         aa(j)=0.0
         bb(j)=dts1
         cc(j)=-2*dts
      else if((j<2+N).AND.(j>2))then
         aa(j)=-dts
         bb(j)=dts1
         cc(j)=-dts
      else if(j==2+N)then
         aa(j)=-2*dts
         bb(j)=dts1+2*Bi/N*dts
         cc(j)=0.0
      end if
    end do

    do j=2,2+N
        theta(j)=theta(j)+gtao
    end do

    call sy(2,2+N,aa,bb,cc,theta)
  end do

  do j=2,2+N
    write(*,*) theta(j)
  end do

end

     subroutine sy(il,iu,bb,dd,aa,cc)
     integer il,iu
     real, dimension(iu) :: aa,bb
     real, dimension(iu) :: cc,dd
   lp = il + 1
   do 10 i = lp,iu
         r = bb(i)/dd(i-1)
         dd(i) = dd(i) - r * aa(i-1)
         cc(i) = cc(i) - r * cc(i-1)
   10 continue
      cc(iu) = cc(iu) / dd(iu)
      do 20 i = lp,iu
         j = iu - i + il
         cc(j) = (cc(j) - aa(j) * cc(j+1)) / dd(j)
   20 continue
      return
      end
