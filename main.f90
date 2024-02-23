program main
    implicit none
    integer :: opcion

    ! Ciclo para el menú
    do
        call mostrarMenu()
        read(*, *) opcion

        select case(opcion)
            case(1)
                print *, "Carga masiva de clientes"
            case(2)
                print *, "Cantidad de ventanillas"
            case(3)
                print *, "Ejecutar paso"
            case(4)
                print *, "Estado en memoria de las estructuras"
            case(5)
                print *, "Reportes"
            case(6)
                print *, "Nombre: Elian Angel Fernando Reyes Yac"
                print *, "Carnet: 202044192"
                print *, "Curso: Estructura de Datos"
            case(7)
                exit ! Salir del programa
            case default
                print *, "Opcion invalida. Por favor, seleccione una opcion valida."
        end select
    end do

contains

    subroutine mostrarMenu()
        print *, " "
        print *, "============ Menu ============"
        print *, "Seleccione una opcion:"
        print *, "1. Carga masiva de clientes"
        print *, "2. Cantidad de ventanillas"
        print *, "3. Ejecutar paso"
        print *, "4. Estado en memoria de las estructuras"
        print *, "5. Reportes"
        print *, "6. Acerca del estudiante"
        print *, "7. Salir"
        print *, "=============================="
    end subroutine mostrarMenu

end program main
