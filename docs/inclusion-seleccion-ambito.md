# Inclusión de casos, selección y ámbito de análisis

LinkEDA distingue entre **apartar un caso** y **centrar temporalmente el análisis en un subconjunto**. La inclusión decide qué casos pueden participar en los análisis. La selección permite señalar casos de forma interactiva; solo actúa como filtro cuando se elige como ámbito de análisis. Una selección guardada conserva los casos elegidos al crearla y puede aplicarse más adelante.

## Tres decisiones distintas

| Decisión | Qué significa | Qué ocurre al cambiarla |
| --- | --- | --- |
| **Casos incluidos** | Son los casos disponibles antes de aplicar un ámbito. Al principio están incluidos todos. | Excluir un caso lo aparta de los análisis posteriores hasta que se vuelva a incluir. |
| **Selección actual** | Son los casos señalados en la hoja o en una vista vinculada. | Se puede modificar continuamente. Señalar casos no reduce por sí solo el ámbito del análisis. |
| **Ámbito de análisis** | Indica con qué casos se trabaja ahora: los incluidos, la selección actual, los no seleccionados o una selección guardada. | Cambiar de ámbito no excluye casos de forma explícita ni borra las selecciones guardadas. |

Los casos disponibles para un análisis son los que cumplen **ambas condiciones**: estar incluidos y pertenecer al ámbito activo. Un análisis concreto puede utilizar menos casos si, por ejemplo, necesita valores que faltan en algunas filas.

## Incluir y excluir desde la hoja de datos

La **diagonal roja sobre el número de caso** indica una exclusión explícita. Un doble clic en ese número alterna entre excluir e incluir el caso. Al excluirlo, LinkEDA también lo retira de la selección actual si estaba señalado, pero mantiene el modo de ámbito que se estuviera aplicando.

El botón **Include all** elimina todas las exclusiones explícitas. No cambia el ámbito activo: si se estaba aplicando una selección guardada, esa selección sigue aplicada.

## Seleccionar y aplicar un ámbito

Se pueden señalar casos sin cambiar el ámbito. Si el ámbito es **Incluidos**, el análisis sigue considerando todos los casos incluidos aunque solo algunos estén señalados.

Al elegir **Selección actual**, el ámbito sigue en vivo los cambios de esa selección: añadir o quitar casos señalados actualiza el subconjunto usado. Al elegir una **selección guardada**, se aplica el conjunto de casos que quedó registrado con su nombre; cambiar después la selección interactiva no modifica ese conjunto guardado.

La columna estrecha **S** de la hoja muestra un **punto azul** en los casos que quedan fuera del ámbito elegido. Ese punto no es una exclusión explícita. Al volver al ámbito **Incluidos**, desaparecen los puntos causados por la selección; las diagonales rojas permanecen hasta volver a incluir sus casos. Un caso puede mostrar a la vez el punto y la diagonal si está fuera del ámbito y además está excluido explícitamente.

## Guardar una selección

**Save selection** toma los casos de la selección actual que estén incluidos, les da un nombre y aplica inmediatamente esa selección guardada como ámbito. Guardarla **no excluye** los demás casos: simplemente quedan fuera de ese ámbito mientras se aplica.

Los casos excluidos **antes** de guardar la selección no entran en ella. Si se excluye un caso **después**, la selección guardada conserva su pertenencia histórica, pero el caso deja de participar mientras permanezca excluido. Al volver a incluirlo, podrá participar de nuevo cuando esa selección guardada esté aplicada. La exclusión tampoco cambia por sí sola el modo de ámbito a **Incluidos**.

## Volver a todos los casos

Para dejar de aplicar una selección actual o guardada, elige **Incluidos** en **Analysis Scope**. Así se utilizan todos los casos que sigan incluidos. Si quieres recuperar también los casos excluidos explícitamente, pulsa **Include all**. Las dos acciones responden a decisiones distintas y pueden realizarse en cualquier orden.

Por ejemplo, con seis casos puedes guardar una selección de los casos 2, 3 y 4. Mientras la aplicas, los casos 1, 5 y 6 muestran un punto en **S**, pero siguen incluidos. Si excluyes explícitamente el caso 3, su número se tacha en rojo y el análisis utiliza los casos 2 y 4. Al volver a **Incluidos**, desaparecen los puntos de **S** y se analizan todos salvo el 3. Al pulsar **Include all**, el caso 3 vuelve a estar disponible; la selección guardada sigue conteniendo 2, 3 y 4.
