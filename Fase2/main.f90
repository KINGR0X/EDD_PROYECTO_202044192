program main
    use json_module
    use matrix_m
    use abb_m
    use lista_album

    implicit none
    
    ! Definir las variables admin y password
    character(len=10) :: admin
    character(len=10) :: password
    
    ! Usuario y clave para el inicio de sesión
    character(len=10) :: usuario
    character(len=10) :: clave

    ! Lo que se va a usar para leer el Json
    type(json_file) :: json   ! Variable de tipo json_file
    type(json_value), pointer :: listPointer, id_capa_pointer, pixelsPointer, imgs_album_Pointer, attributePointer, pixelAttribute  ! punteros
    type(json_core) :: jsonc  ! Se declara una variable del tipo json_core para acceder a las funciones básicas de JSON
    integer:: id_capa, colum, row

    character(:), allocatable  :: color, album_js
    character(len=1000) :: filename
    character(len=1000) :: filename_capas

    integer :: i, p, size, pixels, num  
    logical :: found

    ! Matriz dispersa para las capas
    type(matrix) :: matriz_c

    ! Arbol BB
    type(abb) :: tree_abb

    ! Lista de albumes
    type(lista_circular) :: lista_albumes

    integer :: opcion

    ! Asignar valores a las variables admin y password
    admin = "a"
    password = "1"
    
    ! Ciclo para el menú
    do
        call mostrarMenu()
        read(*, *) opcion

        select case(opcion)
            case(1)
                ! El usuario ingresa su nombre de usuario y contraseña
                print *, "Ingrese su nombre de usuario:"
                read(*, *) usuario
                print *, "Ingrese su contraseña:"
                read(*, *) clave
                
                ! Verificar si el nombre de usuario y la contraseña son del administrador
                if (usuario == admin .and. clave == password) then
                    print *, "*** Bienvenido administrador ***"
                    call mostrarMenuAdmin()
                else
                    ! print *, "Nombre de usuario o contraseña incorrectos"
                    print *, "*** Bienvenido cliente ***"
                    call mostrarMenuCliente()
                end if

            case(2)
                print *, "Registro de usuarios"

            case(3)
                print *, "Saliendo..."
                exit

            case default
                print *, "Opcion invalida. Por favor, seleccione una opcion valida."

        end select
    end do

contains

    subroutine mostrarMenu()
        print *, " "
        print *, "============ Menu ============"
        print *, "Seleccione una opcion:"
        print *, "1. Inicio de sesion"
        print *, "2. Registro de usuarios"
        print *, "3. Salir"
        print *, "=============================="
    end subroutine mostrarMenu

    subroutine mostrarMenuCliente()
        implicit none
        integer :: opcionCliente
        do
            print *, " "
            print *, "============ Menu Cliente ============"
            print *, "Seleccione una opcion:"
            print *, "1. Visualizar reportes de las estructuras"
            print *, "2. Navegacion y gestión de imagenes"
            print *, "3. Carga masiva de capas"
            print *, "4. Carga masiva de imagenes"
            print *, "5. Cargar masiva de albumes"
            print *, "6. Salir"
            print *, "=============================="
            read(*, *) opcionCliente
            select case(opcionCliente)
                case(1)
                    print *, "Visualizando reportes de las estructuras..."
                case(2)
                    print *, "Navegando y gestionando imágenes..."
                case(3)
                    print *, "Cargando masivamente capas..."

                    call cargarCapas()
                    ! call matriz_c%graficar("capas.dot")
                case(4)
                    print *, "Cargando masivamente imágenes..."
                case(5)
                    print *, "Cargando masivamente albumes..."

                    call cargar_albumes()
                case(6)
                    exit
                case default
                    print *, "Opción inválida, por favor seleccione nuevamente."
            end select
        end do
    end subroutine mostrarMenuCliente

    subroutine mostrarMenuAdmin()
        implicit none
        integer :: opcionAdmin
        do
            print *, " "
            print *, "============ Menu Administrador ============"
            print *, "Seleccione una opcion:"
            print *, "1. Arbol B de usuarios (Gráfico)"
            print *, "2. Operaciones sobre los usuarios (insertar, modificar y eliminar)"
            print *, "3. Carga masiva de usuarios"
            print *, "4. Salir"
            print *, "==========================================="
            read(*, *) opcionAdmin
            select case(opcionAdmin)
                case(1)
                    print *, "Mostrando Arbol B de usuarios..."
                case(2)
                    print *, "Realizando operaciones sobre los usuarios..."
                case(3)
                    print *, "=== Carga masiva de usuarios ==="
                    
                    ! El usuario ingresa la direccion del JSON
                    print *, "=== Ingrese la direccion del archivo JSON de clientes: ==="
                    ! read(*, '(A1000)') filename !Ingresar direccion de archivo JSON
                    filename= "C:\Users\elian\OneDrive\Documentos\USAC_2024\EDD_proyectos\&
                    EDD_PROYECTO_202044192\Fase2\clientes.json"

                    call json%initialize()    ! Se inicializa el módulo JSON
                    call json%load(filename=trim(filename))  !se carga el archivo de entrada Json
                    call json%print()         ! Imprimir
                    print *, "Archivo cargado exitosamente."
                    
                    ! call json%info('',n_children=size)

                    ! call json%get_core(jsonc)               ! Se obtiene el núcleo JSON para acceder a sus funciones básicas
                    ! call json%get('', listPointer, found)

                    ! ! print *, "Nombres detectados:"
                    ! do i = 1, size                          ! Se inicia un bucle sobre el número de elementos en el JSON
                    !     call jsonc%get_child(listPointer, i, id_capa_pointer, found = found)  ! Se obtiene el i-ésimo hijo de listPointer
                    !     call jsonc%get_child(id_capa_pointer, 'nombre', attributePointer, found = found)  ! Se obtiene el valor asociado con la clave 'nombre' del hijo actual
                    !     call jsonc%get_child(id_capa_pointer, 'img_g', img_g_pointer, found = found)
                    !     call jsonc%get_child(id_capa_pointer, 'img_p', img_p_pointer, found = found)

                    !     if (found) then                      ! Si se encuentra el valor asociado con la clave 'nombre'
                    !         call jsonc%get(attributePointer, nombre)  ! Se obtiene el valor y se asigna a la variable 'nombre'
                    !         call jsonc%get(img_g_pointer, img_g)
                    !         call jsonc%get(img_p_pointer, img_p)
                    !         ! print *, trim(nombre)           ! Se imprime el nombre sin espacios en blanco adicionales
                    !         call cola_recep%append(nombre,img_g,img_p)  ! Se agrega el nombre a la cola de recepción
                    !     end if
                    ! end do

                    ! call json%destroy() ! Se finaliza el módulo JSON

                case(4)
                    exit
                case default
                    print *, "Opción inválida, por favor seleccione nuevamente."
            end select
        end do
    end subroutine mostrarMenuAdmin

    subroutine cargarCapas()
        implicit none

        ! El usuario ingresa la direccion del JSON
        print *, "=== Ingrese la direccion del archivo JSON de clientes: ==="
        ! read(*, '(A1000)') filename_capas !Ingresar direccion de archivo JSON
        filename_capas= "C:\Users\elian\OneDrive\Documentos\USAC_2024\EDD_proyectos\&
        EDD_PROYECTO_202044192\Fase2\capas.json"

        call json%initialize()    ! Se inicializa el módulo JSON
        call json%load(filename=trim(filename_capas))  !se carga el archivo de entrada Json
        ! call json%print()         ! Imprimir
        print *, "Archivo cargado exitosamente."
                    
        call json%info('',n_children=size)

        call json%get_core(jsonc)               ! Se obtiene el núcleo JSON para acceder a sus funciones básicas
        call json%get('', listPointer, found)

        ! print *, "Nombres detectados:"
        do i = 1, size                          ! Se inicia un bucle sobre el número de elementos en el JSON
            call jsonc%get_child(listPointer, i, id_capa_pointer, found = found)  ! Se obtiene el i-ésimo hijo de listPointer
            call jsonc%get_child(id_capa_pointer, 'id_capa', attributePointer, found = found)  ! Se obtiene el valor asociado con la clave 'nombrdel hijo actual
            
            if (found) then 
                call jsonc%get(attributePointer,id_capa)
                ! Insertar capas en el arbol b
                call tree_abb%insert(id_capa)
            end if

            call jsonc%get_child(id_capa_pointer, 'pixeles', attributePointer, found = found)
            call jsonc%info(attributePointer, n_children=pixels)

            do p = 1, pixels
                call jsonc%get_child(attributePointer, p, pixelsPointer, found = found)
                if (found) then

                    call jsonc%get(pixelsPointer, "color", pixelAttribute, found= found)
                    if (found)then 
                        call jsonc%get(pixelAttribute, color)
                    end if
                    
                    call jsonc%get(pixelsPointer, "columna", pixelAttribute, found= found)
                    if (found)then 
                        call jsonc%get(pixelAttribute, colum)
                    end if

                    call jsonc%get(pixelsPointer, "fila", pixelAttribute, found= found)
                    if (found)then 
                        call jsonc%get(pixelAttribute, row)
                    end if
                end if

                call matriz_c%insert(colum, row, color)
                ! print *, "Capa: ", id_capa, "Color: ", color, "Columna: ", colum, "Fila: ", row

            end do
        end do

        !graficar el arbol b de capas
        ! call tree_abb%graph("arbol_b")

        call json%destroy() ! Se finaliza el módulo JSON

    end subroutine cargarCapas


    subroutine cargar_albumes()
        implicit none

        ! El usuario ingresa la direccion del JSON
        print *, "=== Ingrese la direccion del archivo JSON de clientes: ==="
        ! read(*, '(A1000)') filename_capas !Ingresar direccion de archivo JSON
        filename_capas= "C:\Users\elian\OneDrive\Documentos\USAC_2024\EDD_proyectos\&
        EDD_PROYECTO_202044192\Fase2\albumes.json"

        call json%initialize()    ! Se inicializa el módulo JSON
        call json%load(filename=trim(filename_capas))  !se carga el archivo de entrada Json
        ! call json%print()         ! Imprimir
        print *, "Archivo de albumes cargado exitosamente."
                    
        call json%info('',n_children=size)

        call json%get_core(jsonc)               ! Se obtiene el núcleo JSON para acceder a sus funciones básicas
        call json%get('', listPointer, found)

        ! print *, "Nombres detectados:"
        do i = 1, size                          ! Se inicia un bucle sobre el número de elementos en el JSON
            call jsonc%get_child(listPointer, i, id_capa_pointer, found = found)  ! Se obtiene el i-ésimo hijo de listPointer
            call jsonc%get_child(id_capa_pointer, 'nombre_album', attributePointer, found = found)  ! Se obtiene el valor asociado con la clave 'nombrdel hijo actual
            
            if (found) then 
                call jsonc%get(attributePointer,album_js)
                ! Insertar capas en el arbol b
                print *, "Album: ", album_js
            end if

            call jsonc%get_child(id_capa_pointer, 'imgs', attributePointer, found = found)       
            

                if (found) then
                    call jsonc%info(attributePointer, n_children=pixels)  ! Obtiene el número de elementos en el array 'imgs'

                    do p = 1, pixels
                        call jsonc%get_child(attributePointer, p, pixelsPointer, found = found)  ! Obtiene el p-ésimo elemento del array 'imgs'

                        if (found) then
                            call jsonc%get(pixelsPointer, num)  ! Lee el número
                            print *, "Número: ", num
                        end if
                    end do
                end if
        end do
        !     call jsonc%info(attributePointer, n_children=pixels)

        !     do p = 1, pixels
        !         call jsonc%get_child(attributePointer, p, pixelsPointer, found = found)
                
        !         if (found) then

        !             call jsonc%get(pixelsPointer, "color", pixelAttribute, found= found)
        !             if (found)then 
        !                 call jsonc%get(pixelAttribute, color)
        !             end if
                    
        !         end if

        !     end do
        ! end do

        call json%destroy() ! Se finaliza el módulo JSON

    end subroutine cargar_albumes

end program main
