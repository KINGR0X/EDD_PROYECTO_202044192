program main
    implicit none

    ! Definir las variables admin y password
    character(len=10) :: admin
    character(len=10) :: password

    ! Usuario y clave para el inicio de sesión
    character(len=10) :: usuario
    character(len=10) :: clave

    integer :: opcion


    ! Asignar valores a las variables admin y password
    admin = "admin"
    password = "123"
    
    ! Ciclo para el menú
    do
        call mostrarMenu_principal()
        read(*, *) opcion

        select case(opcion)
            case(1)
                ! El usuario ingresa su nombre de usuario y contraseña
                print *, " "
                print *, "Ingrese el nombre de administrador:"
                read(*, *) usuario
                print *, "Ingrese su contraseña:"
                read(*, *) clave
                        
                ! Verificar si el nombre de usuario y la contraseña son del administrador
                if (usuario == admin .and. clave == password) then
                    print *, "*** Bienvenido administrador ***"
                    call mostrarMenu()
                else
                    print *, " "
                    print *, "XXXX Nombre de usuario o contraseña incorrectos XXXX"
                end if

            case(2)
                    print *, "Saliendo..."
                    exit

            case default
                    print *, "Opcion invalida. Por favor, seleccione una opcion valida."
        end select
    end do


    contains


    subroutine mostrarMenu()
        implicit none 
        integer:: opcion_menu
        do
            print *, " "
            print *, "============ Menu principal ============"
            print *, "Seleccione una opcion:"
            print *, "1. Carga de archivos de sucursales"
            print *, "2. Carga de archivos de rutas"
            print *, "3. Sucursales"
            print *, "4. Reportes"
            print *, "5. Salir"
            print *, "=============================="

            read(*, *) opcion_menu
            select case(opcion_menu)

                case (1)
                    print *, " "
                    print *, "Cargando archivos de sucursales..."
                case (2)
                    print *, " "
                    print *, "Cargando archivos de rutas..."
                case (3)
                    print *, " "
                    print *, "Mostrando sucursales..."
                case (4)
                    print *, " "
                    print *, "Generando reportes..."
                case (5)
                    print *, " "
                    print *, "Saliendo..."
                    exit

            end select    
        end do
    end subroutine mostrarMenu

    subroutine mostrarMenu_principal()
        print *, " "
        print *, "============ Inicio de sesion ============"
        print *, "Seleccione una opcion:"
        print *, "1. Iniciar sesion"
        print *, "2. Salir"
        print *, "=========================================="
    end subroutine mostrarMenu_principal

end program main