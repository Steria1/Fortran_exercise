program exercise1
    implicit none
    real :: temperature, DO_conc
    integer :: i

    open(unit=10, file='saturated_DO.CSV')



    do i = 5, 30, 5
        temperature = real(i)
        DO_conc = calculate_DO(temperature)
        Write(10, 100) i, DO_conc
    100 format(i6, ',', F6.1)    
    end do

    contains
    real function calculate_DO(temperature)
    real, intent(in) ::temperature
    calculate_DO = 14.652 - 0.41022*temperature &
                    + 0.007991*temperature**2 &
                    - 0.000077774*temperature**3
    end function calculate_DO

end program exercise1