!///////////////////////////////////////////////////////////////////////////////
program sir_stochastic
  implicit none

  real, parameter :: beta = 0.001
  real, parameter :: gamma = 0.1
  real :: rate_total, p
  real :: t, dt
  real :: ranval
  integer :: pops, popi, popr
  integer :: i
  character(len=30) :: output

  do i = 1, 100
     t = 0.0
     pops = 190
     popi = 10
     popr = 0
     
     call random_seed

     write(output,9) i
     open(6, file = output)
     write(6,*) t, pops, popi, popr
     do while (t .lt. 100.0)
        rate_total = beta*pops*popi + gamma*popi
        
        call random_number(ranval)
        dt = -1.0*log(ranval)/rate_total
        t = t + dt
        p = beta*pops*popi/rate_total
        if (ranval .lt. p) then
           pops = pops - 1
           popi = popi + 1
        else
           popi = popi - 1
           popr = popr + 1
        end if

        if (pops .le. 0) pops = 0
        if (popi .le. 0) then
           popi = 0
           write(6,*) t, pops, popi, popr
           exit
        end if
        write(6,*) t, pops, popi, popr
     end do
     close(6)
  end do

  write(*,*) ' #Simulation completed'
9 format('output_',i0,'.txt')
end program sir_stochastic
!///////////////////////////////////////////////////////////////////////////////
