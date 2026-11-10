# Instrucciones generales para Codex Cloud

## Alcance

- Resolver la solicitud con el cambio más pequeño que funcione desde el punto de entrada real.
- Preservar la arquitectura, los contratos, el estilo y los cambios locales preexistentes.
- Ajustar el esfuerzo al riesgo. Para una edición puntual, localizar, editar y verificar.
- Ampliar la exploración solo cuando una dependencia, un error o el alcance lo justifique.

## Lectura y herramientas

- Usar primero el archivo, ruta, símbolo o mensaje de error dado por el usuario.
- Buscar coincidencias con `rg -n` en el directorio pertinente. Usar `rg --files` solo si falta la ruta.
- Leer primero el bloque relevante. Ampliar a sus llamadas directas si hace falta entender el flujo.
- Evitar árboles completos, archivos generados y relecturas de contenido que sigue vigente.
- Agrupar consultas independientes. Ejecutar en secuencia las que dependan de un resultado previo.
- Consultar documentación, historial o pruebas cuando resuelvan una duda concreta.
- Revisar `git status --short`, `git diff --stat` y el diff de los archivos pertinentes según necesidad.

## Salida de terminal

- Si RTK ya está disponible y soporta el comando, usarlo para resumir Git, búsquedas y logs.
- Si RTK no está disponible, usar comandos nativos con rutas, rangos y filtros específicos.
- Empezar por el estado, código de salida y fragmento relevante. Ante un fallo, recuperar el error exacto y el contexto necesario.
- Mantener el código de salida al filtrar comandos. Una salida corta o vacía no demuestra éxito.
- Proyectar solo los campos necesarios de JSON. En datos tabulares, preferir esquema, conteos y una muestra acotada.
- Evitar volcar archivos, logs, objetos reactivos, variables de entorno o datos de clientes completos.
- Usar `rtk gain` solo si se solicita medir ahorro. Dejar instalaciones, hooks y proxies fuera del alcance por defecto.

## Cambios de código

- Antes de escribir, usar la primera opción que cubra el caso: código existente del proyecto, función estándar de R, componente nativo de Shiny o bs4Dash, o dependencia ya instalada.
- En el ecosistema Racafé, revisar primero el componente corporativo pertinente si ya forma parte de la app.
- Corregir en el módulo propietario. Mantener IDs, namespaces, contratos reactivos y reglas de negocio.
- Evitar helpers de un solo uso, abstracciones, dependencias y archivos nuevos sin necesidad concreta.
- Mantener comentarios, formato y convenciones del código existente. Limitar el diff a las líneas del cambio.
- Preservar validaciones, manejo de errores, accesibilidad y controles de acceso.

## Validación

- Para cambios pequeños, revisar el diff y hacer como máximo una comprobación sintáctica focalizada.
- Si cambia lógica de negocio, permisos, escrituras o contratos reactivos, validar de forma puntual el comportamiento afectado.
- Reservar suites completas, pruebas de integración, E2E y navegador para solicitudes expresas.
- Evitar instalar dependencias solo para pruebas o repetir una comprobación sin cambios relevantes.
- Distinguir entre código que parsea, comportamiento comprobado y validación pendiente.

## Documentación y comunicación

- Crear o actualizar documentación únicamente por solicitud expresa.
- Informar la rama de trabajo y los casos dudosos conservados al entregar una limpieza.
- Evitar subagentes para tareas simples y mantener los avances breves, centrados en hallazgos, decisiones o bloqueos.
- Usar español técnico y directo. Conservar literales los comandos, rutas, identificadores y errores relevantes.
- Si se pide un ajuste puntual, entregar el fragmento exacto. Si se pide el script completo, entregarlo completo.
- Evitar repetir el pedido, narrar pasos rutinarios o reproducir en el chat un diff ya disponible.

## Entrega

Informar archivos modificados o eliminados, cambio realizado, validación ejecutada
y pendientes relevantes. Máximo 150 palabras en la respuesta final, salvo
solicitud expresa de mayor detalle o de un entregable completo.
