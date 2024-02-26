program main
    use json_module

    use cola_recepcion
    use List_of_list_vent
    
    implicit none

    type(cola) :: cola_recep
    type(List_of_list) :: list_ventanillas


    ! Lo que se va a usar para leer el Json
    type(json_file) :: json   ! Variable de tipo json_file
    type(json_value), pointer :: listPointer, personPointer, img_g_pointer,img_p_pointer, attributePointer  ! punteros
    type(json_core) :: jsonc  ! Se declara una variable del tipo json_core para acceder a las funciones básicas de JSON
    character(:), allocatable :: nombre
    integer :: img_g,img_p
    character(len=100) :: filename
    integer :: i, size       
    logical :: found

    ! ===============================
    integer :: opcion
    integer:: c_ventanillas
    character(len=10) :: ventanilla_name

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

                ! Creación de ventanillas segun la cantidad ingresada
                do i = 1, c_ventanillas
                    call list_ventanillas%insert(i,'vacio')
                    ! cada ventanilla tiene su propia pila

                    ! Pop usado para edjar la pila vacia
                    call list_ventanillas%popAtIndex(i)
                    call list_ventanillas%popAtIndex(i)
                end do
                
            case(2)
                print *, "Ejecutar paso"
                ! Primero se verifica si hay clientes en la cola de recepcion para evitar que en el primer paso se asigne cliente y agrege una imegen
                call list_ventanillas%imagenes_a_ventanilla()
                call cliente_a_ventanilla()

            case(3)
                print *, "Estado en memoria de las estructuras"
                call cola_recep%print()
                print *, " "
                print *, "Lista ventanillas: "
                call list_ventanillas%printList()
                
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

    !subroutine para asignar cliente a ventanilla
    subroutine cliente_a_ventanilla()
        !Primero la cola de recepcion devuelve los valores del primer cliente y se guardan en variables temporales
        character(len=:), allocatable:: cliente_temp
        integer:: img_g_temp
        integer:: img_p_temp
        integer:: ventanilla_desocupada

        cliente_temp= cola_recep%return_cliente_name()
        img_g_temp= cola_recep%return_cliente_img_g()
        img_p_temp= cola_recep%return_cliente_img_p()


        ! Encontrar el primer nodo desocupado
        ventanilla_desocupada = list_ventanillas%buscar_nodo_desocupado()
        
        ! Imprimir el resultado
        if (ventanilla_desocupada > 0) then
            print *, 'El primer nodo desocupado tiene el índice:', ventanilla_desocupada
            call list_ventanillas%asignar_datos_cliente(ventanilla_desocupada, cliente_temp,img_g_temp, img_p_temp)
            !Se elimina el primer nodo de la cola de recepcion, solo si una ventanilla esta disponible
            call cola_recep%delete()
        else
            print *, 'No se encontraron nodos desocupados.'    
        end if

    end subroutine cliente_a_ventanilla

end program main




