!///////////////////////////////////////////////////////////////////////////////
program sir_stochastic
  implicit none

  real, parameter :: beta = 0.001
  real, parameter :: gamma = 0.1
  real :: rate_total, p
  real :: t, dt
  real :: ranval
  integer :: pops, popi, popr
  integer :: i, t_int
  character(len=30) :: output, output_int

  do i = 1, 100
     write(*,*) i
     t = 0.0
     t_int = 0
     pops = 190
     popi = 10
     popr = 0
     
     call random_seed

     write(output,9) i
     write(output_int,19) i
     open(6, file = output)
     open(16, file = output_int)
     write(6,*) t, pops, popi, popr
     write(16,*) t_int, pops, popi, popr
     t_int = t_int + 1

     do while (t .lt. 100.0)
        rate_total = beta*pops*popi + gamma*popi

        if (rate_total .gt. 0.0) then
           call random_number(ranval)
           dt = -1.0*log(ranval)/rate_total
        else
           dt = 1000000.0
        end if

        do while (t_int .lt. t + dt)
           if (t_int .gt. 100.0) exit
           write(16,*) t_int, pops, popi, popr
           t_int = t_int + 1
        end do
        
        t = t + dt
        if (t .gt. 100.0) exit
        
        p = beta*pops*popi/rate_total
        if (ranval .lt. p) then
           pops = pops - 1
           popi = popi + 1
        else
           popi = popi - 1
           popr = popr + 1
        end if

        write(6,*) t, pops, popi, popr
     end do
     close(6)
     close(16)
  end do

  write(*,*) ' #Simulation completed'
9 format('output_',i0,'.txt')
19format('output_tint_',i0,'.txt')
end program sir_stochastic
!///////////////////////////////////////////////////////////////////////////////
