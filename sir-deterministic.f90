!/////////////////////////////////////////////////////////////////////
module parameters
  real, parameter :: beta = 0.001
  real, parameter :: gamma = 0.1
  real :: s, i, r, t
  real :: tstep, nsteps
end module parameters
!/////////////////////////////////////////////////////////////////////
program sir
  use parameters
  implicit none

  integer :: j

  s = 190.0
  i = 10.0
  r = 0.0
  tstep = 0.1
  nsteps = 1000
  
  open(6, file = 'sir-deterministic.txt')
  write(6,*) 0, s, i, r
  do j = 1, nsteps
     t = j*tstep
     call rk4
     write(6,*) t, s, i, r
  end do
  close(6)

  ! call system('gnuplot -p sir.gnu')
end program sir
!/////////////////////////////////////////////////////////////////////
subroutine rk4
  use parameters
  implicit none

  real :: h
  real :: derivS, s1, s2, s3, s4
  real :: derivI, i1, i2, i3, i4
  real :: derivR, r1, r2, r3, r4

  h = tstep/2.0
  
  s1 = tstep * derivS(t, s, i, r)
  i1 = tstep * derivI(t, s, i, r)
  r1 = tstep * derivR(t, s, i, r)
  
  s2 = tstep * derivS(t+h, s+s1/2.0, i+i1/2.0, r+r1/2.0)
  i2 = tstep * derivI(t+h, s+s1/2.0, i+i1/2.0, r+r1/2.0)
  r2 = tstep * derivR(t+h, s+s1/2.0, i+i1/2.0, r+r1/2.0)
  
  s3 = tstep * derivS(t+h, s+s2/2.0, i+i2/2.0, r+r2/2.0)
  i3 = tstep * derivI(t+h, s+s2/2.0, i+i2/2.0, r+r2/2.0)
  r3 = tstep * derivR(t+h, s+s2/2.0, i+i2/2.0, r+r2/2.0)
  
  s4 = tstep * derivS(t+tstep, s+s3, i+i3, r+r3)
  i4 = tstep * derivI(t+tstep, s+s3, i+i3, r+r3)
  r4 = tstep * derivR(t+tstep, s+s3, i+i3, r+r3)

  s = s + (s1 + (2.0*(s2 + s3)) + s4)/6.0
  i = i + (i1 + (2.0*(i2 + i3)) + i4)/6.0
  r = r + (r1 + (2.0*(r2 + r3)) + r4)/6.0
end subroutine rk4
!/////////////////////////////////////////////////////////////////////
function derivS(tdummy, sdummy, idummy, rdummy)
  use parameters
  real :: tdummy, sdummy, idummy, rdummy
  
  derivS = -1.0*beta*sdummy*idummy
end function derivS
!/////////////////////////////////////////////////////////////////////
function derivI(tdummy, sdummy, idummy, rdummy)
  use parameters
  real :: tdummy, sdummy, idummy, rdummy

  derivI = beta*sdummy*idummy - gamma*idummy
end function derivI
!/////////////////////////////////////////////////////////////////////
function derivR(tdummy, sdummy, idummy, rdummy)
  use parameters
  real :: tdummy, sdummy, idummy, rdummy

  derivR = gamma*idummy
end function derivR
!/////////////////////////////////////////////////////////////////////
