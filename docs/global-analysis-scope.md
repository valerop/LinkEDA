# Ámbito global de análisis — informe del refactor

## 1. Archivos modificados

El trabajo se ha hecho en `LinkEDA-mac-verify`, conservando los cambios anteriores del proyecto.

- Núcleo compartido: `src/core/analysis_scope.{h,cpp}`, `application_state.{h,cpp}`, `command_model.cpp`, `command_dispatcher.cpp`, `glm_model.cpp`, `provenance_model.cpp`, `linkeda_document.cpp`.
- macOS: `src/platform/macos/linkeda_macos_app.mm`.
- Windows: `src/platform/windows/winui/LinkEDA/App.xaml.{h,cpp}`, `ApplicationMenu.{h,cpp}`, `WorkflowWindows.{h,cpp}`.
- R: `selection.R`, `datasets.R`, `datadesk.R`, `state.R`, `glm_model.R`, `generalized_linear_model.R`, `regression_comparison.R`, `mixed_models.R`, `compare_means.R`, `correlation_matrix.R`, `dimensionality.R`, `quick_cluster.R`, `scale_analysis.R`, `table1.R`, `contingency_table.R`, `mi_analysis.R`, `missing_data_overview.R`, `missing_data_imputation.R`, `provenance.R`.
- Pruebas nuevas: `tests/core/global_analysis_scope_test.cpp`, `tests/native/global_analysis_scope_smoke.mm`, `tests/testthat/test-global-analysis-scope.R`.
- Pruebas adaptadas: `analysis_scope_test.cpp`, `test-correlation-scope-consistency.R`, `test-dimensionality.R`, `test-missing-data-imputation.R`, `test-regression-scope.R`.

## 2. Arquitectura anterior

`ApplicationState` ya almacenaba un ámbito por conjunto de datos, separado de la selección. Sin embargo, diferentes ventanas mantenían además un campo `scope`, un selector editable y su propia lógica para obtener filas seleccionadas. Algunas rutas R consultaban la selección directamente y otras trabajaban sobre todos los datos. Las comparaciones generalizadas podían conservar el ámbito de cada modelo individual.

## 3. Fuente única de verdad

Se reutiliza `ApplicationState::activeAnalysisScope`, sin introducir otro estado global. `captureAnalysisScope` captura su valor al comenzar una ejecución y deriva de él los campos antiguos del protocolo.

Modos centrales:

- Todos los casos.
- Selección actual: sigue la selección interactiva.
- Casos no seleccionados: sigue el complemento de la selección.
- Selecciones guardadas y subconjuntos explícitos existentes: conservan sus identificadores de fila.

En R, `.rls_capture_analysis_scope` y `.rls_apply_scope_to_model_request` son los adaptadores comunes. Las llamadas directas durante una sesión consultan el ámbito global. Una solicitud nativa ya encolada utiliza sus filas capturadas; no vuelve a consultar una selección que podría haber cambiado.

## 4. Controles locales

Los selectores de correlaciones, componentes/factores, comparaciones de medias, modelos lineales/generalizados y comparaciones entre modelos se convierten en indicadores de solo lectura. Las opciones locales de ámbito de los menús se retiran o rechazan explícitamente como comandos antiguos.

macOS y Windows actualizan desde la misma notificación tanto los indicadores como las vistas vivas. Los gráficos exploratorios se reconstruyen y los análisis con ajuste automático vuelven a calcularse con el nuevo ámbito. El control central, incluido el menú de la barra de estado de macOS, sigue siendo el lugar para cambiarlo.

Un gráfico exploratorio puede desactivar **Auto-update with Global Scope**. En ese momento el ámbito se convierte en una instantánea explícita de identificadores de fila y la ventana queda marcada como **Frozen**. Los modelos usan el control **Auto-fit** con la misma semántica: al desactivarlo mantienen visible el último resultado y explican que corresponde al ámbito anterior. Sus diagnósticos permanecen ligados a ese ajuste congelado.

## 5. Selección y ámbito

`groupSelections_` representa el marcado interactivo; `activeAnalysisScopes_`, la política global. Seleccionar 20 puntos con ámbito «All» deja disponibles las 100 observaciones. Solamente los modos «Selected» y «Unselected» dependen de los cambios de selección.

Una selección guardada sigue siendo fija. No se convierte en una selección viva porque cambie el marcado. El ámbito explícito vacío sigue siendo vacío; las rutas de ajuste lo rechazan, sin sustituirlo por todos los casos.

Los casos incluidos forman el universo previo a cualquier ámbito. Al excluir casos, salen del marcado actual y de los análisis posteriores sin cambiar el tipo de ámbito activo. Una selección guardada conserva los miembros que tenía al crearse: las exclusiones posteriores los filtran mientras duren y, al volver a incluirlos, pueden reaparecer en ese ámbito. Al crear una selección guardada se omiten los casos que ya estaban excluidos.

## 6. Muestra efectiva

La secuencia es: datos completos → ámbito global → requisitos del análisis → filas efectivamente utilizadas.

Los registros R y la procedencia compartida separan filas solicitadas, utilizadas y excluidas. Cuando el resultado proporciona las filas utilizadas, se muestran también `Analysis scope N`, `analyzed N` y `analysis-specific exclusions`. No se atribuyen todas las exclusiones a valores ausentes: pueden existir otros requisitos del modelo.

Las tablas con muestras diferentes por variable o pareja conservan sus N por resultado; no se inventa una muestra única para toda la tabla.

## 7. Instantáneas de resultados

Cada ejecución conserva el ámbito, los identificadores estables de fila y la versión de datos. El indicador de una ventana muestra el ámbito actual; `Scope when computed` identifica el del resultado aceptado.

Cambiar el ámbito global reconstruye los gráficos exploratorios vivos y solicita un nuevo ajuste para los análisis vivos. La solicitud captura el ámbito vigente y, cuando R devuelve el resultado, sustituye el contenido anterior de esa misma ventana. Una solicitud ya encolada conserva sus filas y un resultado tardío de una revisión anterior no puede sobrescribir la revisión vigente.

Una vista congelada es la excepción explícita: conserva su geometría o resultado anterior, su procedencia y sus vistas derivadas. No se presenta el ámbito global nuevo como si fuese el ámbito con el que se calculó.

## 8. Exportación de R y publicación

Ambas rutas siguen utilizando la procedencia compartida del resultado. El exportador prepara datos de la versión histórica y de las filas solicitadas por la instantánea; no consulta la selección actual.

El código identifica el ámbito, su N y la muestra efectiva. Los paquetes de verificación incluyen los datos ya restringidos al ámbito capturado y el código público de R que vuelve a ajustar el análisis y aplica su tratamiento de valores ausentes. No se exporta una llamada a un selector local ni se presenta la selección actual como si fuese la histórica.

La prueba ejecutable calcula un modelo con ámbito N = 80 y muestra efectiva N = 70, cambia el ámbito global y ejecuta la receta del resultado anterior: recupera el mismo N efectivo y los mismos coeficientes.

## 9. Missing Data e imputación múltiple

Missing Data Overview y los modelos de missingness capturan el ámbito antes de definir patrones/indicadores. Sobre objetos MI reconocidos usan los datos originales incompletos cuando se solicita esa fuente.

La imputación recibe únicamente las filas del ámbito. Se preservan los identificadores originales y estables. Los subconjuntos de MI se aplican a datos originales, máscara de ausencia y todos los conjuntos completados; se admiten las representaciones existentes de la máscara como tabla, matriz o lista de columnas.

Las variables que se imputan, los predictores y las variables que definen patrones siguen siendo opciones distintas del ámbito. El cálculo estadístico de estas rutas sigue en R (`mice`, `naniar`, `stats`, `psych` y los motores ya utilizados por cada modelo); este refactor no introduce estimadores ni fórmulas estadísticas en C++.

## 10. Operaciones sobre datos

Renombrar, cambiar tipos, añadir columnas y otras operaciones de datos conservan su semántica explícita. El ámbito no se aplica como filtro oculto a esas operaciones.

Guardar indicadores o predicciones de missingness sigue alineando el resultado con las filas originales y dejando ausentes las filas no calculadas. Guardar puntuaciones de escalas utiliza asimismo los identificadores originales del resultado. Sincronizar una tabla calculada sobre un subconjunto no sustituye el conjunto completo registrado por ese subconjunto.

## 11. Compatibilidad

Los campos antiguos `scope`/`dataScope` del protocolo y los documentos se mantienen como representación de solicitudes y resultados. Al ejecutar, el campo antiguo se deriva del estado central: no puede imponer otro ámbito.

La API antigua `ls_model_scope` modifica explícitamente el ámbito global del conjunto de datos. En R sin una sesión nativa, los argumentos explícitos siguen siendo utilizables para trabajo por lotes.

Los documentos anteriores continúan siendo legibles. Se amplía la lectura del origen del ámbito para «Unselected». Al reabrir un ámbito vivo se reconstruye su selección subyacente a partir de las filas guardadas; los resultados históricos conservan sus instantáneas.

## 12. Validación

- Compilación completa del núcleo compartido y del ejecutable macOS.
- Pruebas C++: `analysis_scope_test`, `global_analysis_scope_test`, `provenance_model_test`, `missing_data_model_test`.
- Prueba nativa Cocoa: indicador no editable, All100 con selección20, Selected20, actualización de un gráfico vivo al cambiar a70, conservación al congelarlo, ámbito nombrado de3, otro ámbito nombrado de2, rechazo de ámbito vacío y vuelta aAll100 sin alterar solicitudes ya encoladas.
- Pruebas R de ámbito global, ámbito de regresión, correlaciones, componentes/factores, comparaciones de medias, Table 1, contingencia MI, Missing Data, imputación, modelos mixtos y procedencia/exportación.
- Pruebas adicionales de escalas y agrupamiento sobre un ámbito de40 filas.
- Prueba de guardado, lectura y restauración de un ámbito Unselected, además de lectura de documento versión7.
- `git diff --check`.
- Instalación completada con `R CMD INSTALL --no-configure`. Una sesión nueva del paquete instalado verificó ámbito80, analizados70 y excluidos10.
- SHA-256 del ejecutable compilado e instalado: `be18b95d24bd305ca1d7238e5dc07ce64c33e7517bf7cd51a4a7d10a5e777b33`.

La batería de imputación contiene una prueba externa que se omite cuando no se proporciona `LINKEDA_MI_REFERENCE_RDS`; no se cuenta como validada. Las advertencias de ajustes perfectos y eventos registrados en los datos de prueba se conservan.

## 13. Límites conocidos

- No hay entorno Windows/WinUI en este Mac: su implementación se ha adaptado y revisado, pero falta compilarla y probarla en Windows. La equivalencia del núcleo se comprueba con las pruebas compartidas; la interfaz Windows no se declara validada.
- Los informes de missingness se apoyan en conjuntos de datos instantánea identificados como tales. Conservan los datos originales y el ámbito del momento de creación. Para rehacer patrones o el modelo de missingness sobre otro ámbito del conjunto fuente se vuelve a solicitar el análisis desde ese conjunto; cambiar el ámbito del padre no reescribe una instantánea histórica.
- Las salidas antiguas sin procedencia suficiente no adquieren retrospectivamente metadatos que no se guardaron. Una nueva ejecución genera la información actual.
- La recarga del ejecutable y del paquete requiere reiniciar la sesión de LinkEDA/R. No se cierran automáticamente las ventanas del usuario ni se descartan resultados abiertos.
