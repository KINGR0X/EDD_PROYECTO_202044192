program main
    use json_module

    use cola_recepcion
    use List_of_list_vent
    use cola_impresion
    use lista_clientes_espera
    use lista_clientes_atentidos
    
    implicit none

    type(cola) :: cola_recep
    type(List_of_list) :: list_ventanillas

    !colas de impresion (impresoras)
    type(cola_im) :: cola_img_grande
    type(cola_im) :: cola_img_pequena

    !cola de espera
    type(lista_circular) :: lista_espera 

    ! Lista simple clientes atendidos
    type(lista_simple_atendidos):: lista_c_atendidos


    ! Lo que se va a usar para leer el Json
    type(json_file) :: json   ! Variable de tipo json_file
    type(json_value), pointer :: listPointer, personPointer, img_g_pointer,img_p_pointer, attributePointer  ! punteros
    type(json_core) :: jsonc  ! Se declara una variable del tipo json_core para acceder a las funciones básicas de JSON
    character(:), allocatable :: nombre
    integer :: img_g,img_p
    character(len=100) :: filename, cliente_a_graficar
    integer :: i, size       
    logical :: found

    ! ===============================
    integer :: opcion
    integer:: c_ventanillas
    character(len=10) :: ventanilla_name
    character(len=:), allocatable :: valor_eliminado
    integer:: num_pasos
    integer:: index_espera
    integer:: index_atendidos

    num_pasos = 0
    index_espera = 0
    index_atendidos = 0

    ! Ciclo para el menú
    do
        call mostrarMenu()
        read(*, *) opcion

        select case(opcion)
            case(1)
                ! El usuario ingresa la direccion del JSON
                print *, "=== Ingrese la direccion del archivo JSON: ==="
                read(*, '(A100)') filename
                ! filename= "C:\Users\elian\Downloads\entrada\entrada.json"

                call json%initialize()    ! Se inicializa el módulo JSON
                call json%load(filename=trim(filename))  !se carga el archivo de entrada Json
                ! call json%print()         ! Imprimir
                print *, "Archivo cargado exitosamente."
                
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
                    call list_ventanillas%pop_inicial(i)
                    call list_ventanillas%pop_inicial(i)
                end do
                num_pasos = 0
            case(2)
                !se suma un paso
                num_pasos = num_pasos + 1
                print '(A,I2,A)', "--------------- Paso ", num_pasos, " ---------------"

                call clientes_a_atendidos()

                !Esto espera un paso para entregar la imagen impresa
                call entregar_img_impresas()

                ! Esto espera un paso al cliente
                call img_a_impresora()

                ! Primero se verifica si hay clientes en la cola de recepcion para evitar que en el primer paso se asigne cliente y agrege una imegen
                call list_ventanillas%imagenes_a_ventanilla()

                call cliente_a_ventanilla()

            case(3)
                print *, "Estado en memoria de las estructuras"
                call cola_recep%graficar("cola_recepcion.dot")
                call list_ventanillas%graficar("lista_ventanillas.dot")
                call cola_img_grande%graficar("cola_img_grande.dot", "Impresora Grande")
                call cola_img_pequena%graficar("cola_img_pequena.dot", "Impresora pequeña")

                call lista_espera%graficar("lista_espera.dot")
                call lista_c_atendidos%graficar("lista_c_atendidos.dot")
            
            case(4)
                call abrirpdf()

            case(5)
                !Ingrese el nombre del cliente que desea graficar
                print *, "=== Ingrese el nombre del cliente del que desea obtener la informacion ==="
                read(*, '(A100)') cliente_a_graficar
                
                ! print *, "Reporte del cliente ", cliente_a_graficar

                call lista_c_atendidos%graficar_por_nombre("reporte_cliente.dot",cliente_a_graficar)
                

                call lista_c_atendidos%graficar_nodo_max_pasos("cliente_mas_pasos.dot")
                call abrirReporte()
                
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
        print *, "1. Ingresar parametros iniciales"
        print *, "2. Ejecutar paso"
        print *, "3. Estado en memoria de las estructuras"
        print *, "4. Abrir archivos PDF"
        print *, "5. Reportes"
        print *, "6. Acerca del estudiante"
        print *, "7. Salir"
        print *, "=============================="
    end subroutine mostrarMenu

    subroutine abrirReporte()
        character(len=100) :: command

        ! Reemplaza 'nombre_archivo.pdf' con la ruta de tu archivo PDF
        command = "reporte_cliente.dot.pdf"
        call system(command)
        command = "cliente_mas_pasos.dot.pdf"
        call system(command)

    end subroutine abrirReporte

    subroutine abrirpdf()
        character(len=100) :: command

        ! Reemplaza 'nombre_archivo.pdf' con la ruta de tu archivo PDF
        command = "cola_recepcion.dot.pdf"
        call system(command)
        command = "lista_ventanillas.dot.pdf"
        call system(command)
        command = "cola_img_grande.dot.pdf"
        call system(command)
        command = "cola_img_pequena.dot.pdf"
        call system(command)
        command = "lista_espera.dot.pdf"
        call system(command)
        command = "lista_c_atendidos.dot.pdf"
        call system(command)
    end subroutine abrirpdf

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
        
        if (ventanilla_desocupada > 0) then
            ! print *, 'Esta desocupada la ventanilla:', ventanilla_desocupada
            !Se elimina el cliente de la cola de recepcion
            call cola_recep%delete()
            call list_ventanillas%asignar_datos_cliente(ventanilla_desocupada, cliente_temp,img_g_temp, img_p_temp)
            ! print *, "El cliente ", cliente_temp, " ingresa a la ventanilla",ventanilla_desocupada
            !Se elimina el primer nodo de la cola de recepcion, solo si una ventanilla esta disponible
            
        ! else
            ! print *, 'No se encontraron nodos desocupados.'    
        end if

    end subroutine cliente_a_ventanilla

    subroutine img_a_impresora()
        integer:: x
        logical :: pila_llena_ventanilla
        pila_llena_ventanilla= .false.

        ! Ciclo para verificar si el nodo está ocupado
        do x=1,c_ventanillas

            pila_llena_ventanilla= list_ventanillas%verificar_nodo(x)

            if (pila_llena_ventanilla) then

                ! Cuando la pila esta llena se va el cliente a la lista de espera
                call cliente_a_espera(x)

                !Ciclo usando check_stack_size para eliminar todos los elementos de la pila
                do while (list_ventanillas%check_stack_size(x)>0)
                    valor_eliminado = list_ventanillas%popAtIndex(x)

                    ! Se revisa si el valor_elimiado es igual a "Imagen Grande"
                    if (valor_eliminado == "Imagen Grande") then
                        call cola_img_grande%append(valor_eliminado)
                        print *, "La impresora grande recibio una imagen"
                    else if (valor_eliminado == "Imagen Pequena") then
                        call cola_img_pequena%append(valor_eliminado)
                        print *, "La impresora pequena recibio una imagen"
                    end if
                end do
            end if
        end do
    end subroutine img_a_impresora

    subroutine cliente_a_espera(n_nodo)
        character(:), allocatable :: nombre, nombre_ventanilla
        integer:: num_ventanilla, n_imgG, n_imgP,n_nodo

        ! se le suma 1 al index_espera
        index_espera = index_espera + 1


        nombre= list_ventanillas%return_nombre_cliente(n_nodo)
        num_ventanilla= list_ventanillas%return_num_ventanilla(n_nodo)
        n_imgG= list_ventanillas%return_num_imgG(n_nodo)
        n_imgP= list_ventanillas%return_num_imgP(n_nodo)


        ! call list%append(1, "Cliente1", "Ventanilla1", 2, 1, 6)
        call lista_espera%append(index_espera,nombre,num_ventanilla,n_imgG, n_imgP, num_pasos)
        print *, "El cliente ", nombre, " pasa a la lista de espera"
        ! call lista_espera%print()

    end subroutine cliente_a_espera

    subroutine entregar_img_impresas()
        integer:: w

        ! Se entrega la imagen impresa
        w=lista_espera%append_img()

        ! se elimina imagen de la cola de impresion
        if (w == 1) then
            call cola_img_grande%delete()
            ! print *, "La impresora grande entrego una imagen"
        else if (w == 2) then
            call cola_img_pequena%delete()
            ! print *, "La impresora pequena entrego una imagen"
        end if

    end subroutine entregar_img_impresas

    subroutine clientes_a_atendidos()
        integer :: z, size_lista_espera, min_espera
        integer :: e_img_g, e_img_p, e_num_v, espera_num_pasos
        character(len=:), allocatable :: e_nombre_cliente

        size_lista_espera = lista_espera%get_size()

        ! Asegurarse de que la lista de espera tenga al menos un cliente
        if (size_lista_espera > 0) then

            min_espera= lista_espera%get_min_value()

            if (min_espera>0) then
                z= min_espera
                if (lista_espera%verificar_completo(z) .eqv. .true.)then 
                    e_img_g = lista_espera%get_img_g(z)
                    e_img_p = lista_espera%get_img_p(z)
                    e_nombre_cliente = lista_espera%get_name_cliente(z)
                    e_num_v = lista_espera%get_num_ventanilla(z)
                    index_atendidos = index_atendidos + 1
                    print *, "El cliente ", e_nombre_cliente, " fue atendido"
                    call lista_c_atendidos%append(index_atendidos, e_nombre_cliente, e_num_v, e_img_g, e_img_p, num_pasos)
                    call lista_espera%delete(z)
                end if
            end if
        end if
    end subroutine clientes_a_atendidos
    
end program main
