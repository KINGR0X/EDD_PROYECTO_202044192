program main
    use cola_recepcion

    use json_module
    implicit none

    type(cola) :: cola_recep

    ! Lo que se va a usar para leer el Json
    type(json_file) :: json   ! Variable de tipo json_file
    type(json_value), pointer :: listPointer, personPointer, img_g_pointer,img_p_pointer, attributePointer  ! punteros
    type(json_core) :: jsonc  ! Se declara una variable del tipo json_core para acceder a las funciones básicas de JSON
    character(:), allocatable :: nombre,img_g,img_p  ! Se declara una cadena de caracteres que se asignará dinámicamente
    character(len=100) :: filename
    integer :: i, size       
    logical :: found

    ! ===============================
    integer :: opcion
    integer:: c_ventanillas

    ! Ciclo para el menú
    do
        call mostrarMenu()
        read(*, *) opcion

        select case(opcion)
            case(1)
                ! El usuario ingresa la direccion del JSON
                print *, "=== Ingrese la direccion del archivo JSON: ==="
                ! read(*, '(A100)') filename
                filename= "C:\Users\elian\Downloads\entrada\entrada.json"

                call json%initialize()    ! Se inicializa el módulo JSON
                call json%load(filename=trim(filename))  !se carga el archivo de entrada Json
                ! call json%print()         ! Imprimir
                
                call json%info('',n_children=size)

                call json%get_core(jsonc)               ! Se obtiene el núcleo JSON para acceder a sus funciones básicas
                call json%get('', listPointer, found)

                ! print *, "Nombres detectados:"
                do i = 1, size                          ! Se inicia un bucle sobre el número de elementos en el JSON
                    call jsonc%get_child(listPointer, i, personPointer, found = found)  ! Se obtiene el i-ésimo hijo de listPointer
                    call jsonc%get_child(personPointer, 'nombre', attributePointer, found = found)  ! Se obtiene el valor asociado con la clave 'nombre' del hijo actual
                    call jsonc%get_child(personPointer, 'img_g', img_g_pointer, found = found)
                    call jsonc%get_child(personPointer, 'img_p', img_p_pointer, found = found)

                    if (found) then                      ! Si se encuentra el valor asociado con la clave 'nombre'
                        call jsonc%get(attributePointer, nombre)  ! Se obtiene el valor y se asigna a la variable 'nombre'
                        call jsonc%get(img_g_pointer, img_g)
                        call jsonc%get(img_p_pointer, img_p)
                        ! print *, trim(nombre)           ! Se imprime el nombre sin espacios en blanco adicionales
                        call cola_recep%append(nombre,img_g,img_p)  ! Se agrega el nombre a la cola de recepción
                    end if
                end do

                call json%destroy()                    ! Se finaliza el módulo JSON

                ! Ingresar la cantidad de ventanillas
                print *, " "
                print *, "=== Ingrese la cantidad de ventanillas ==="
                read(*, *) c_ventanillas
                
            case(2)
                print *, "Ejecutar paso"

            case(3)
                print *, "Estado en memoria de las estructuras"
                call cola_recep%print()
            case(4)
                print *, "Reportes"
            case(5)
                print *, "Nombre: Elian Angel Fernando Reyes Yac"
                print *, "Carnet: 202044192"
                print *, "Curso: Estructura de Datos"
            case(6)
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
        print *, "1. Ingresar parametros iniciales"
        print *, "2. Ejecutar paso"
        print *, "3. Estado en memoria de las estructuras"
        print *, "4. Reportes"
        print *, "5. Acerca del estudiante"
        print *, "6. Salir"
        print *, "=============================="
    end subroutine mostrarMenu



end program main




