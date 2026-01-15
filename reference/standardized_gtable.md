# Standardized gtable Representation

The standardized gtable representation ensures that all plots share a
consistent row–column structure, facilitating flexible composition,
patch insertion, and alignment across multiple plots.

This layout provides a fixed-size gtable grid for predictable
positioning of plot components such as titles, axes, legends, captions,
and margins. It is used internally for layout normalization and
patch-based alignment.

## Row structure (top -\> bottom)

|       |                      |                                    |
|-------|----------------------|------------------------------------|
| Index | Component            | Description                        |
| 1     | `margin-top`         | External top spacing               |
| 2     | `tag-top`            | Top tag (e.g., "A", "B")           |
| 3     | `title`              | Main title                         |
| 4     | `subtitle`           | Subtitle                           |
| 5     | `guide-box-top`      | Legend box at top                  |
| 6     | `guide-box-spacing`  | Space between legend and main area |
| 7     | `patch-title-top`    | Top patch title                    |
| 8     | `xlab-top`           | Top x-axis label (rare)            |
| 9     | `axis-top`           | Top axis ticks and labels          |
| 10    | `strip-top`          | Top strip (facet label)            |
| 11    | `panel`              | Main plotting panel                |
| 12    | `strip-bottom`       | Bottom strip (facet label)         |
| 13    | `axis-bottom`        | Bottom axis ticks and labels       |
| 14    | `xlab-bottom`        | Bottom x-axis label                |
| 15    | `patch-title-bottom` | Top patch title                    |
| 16    | `guide-box-spacing`  | Space before bottom legend box     |
| 17    | `guide-box-bottom`   | Bottom legend box                  |
| 18    | `caption`            | Caption or footnote text           |
| 19    | `tag-bottom`         | Bottom tag (optional)              |
| 20    | `margin-bottom`      | External bottom spacing            |

## Column structure (left -\> right)

|       |                     |                                |
|-------|---------------------|--------------------------------|
| Index | Component           | Description                    |
| 1     | `margin-left`       | External left spacing          |
| 2     | `tag-left`          | Optional side tag              |
| 3     | `guide-box-left`    | Left legend box                |
| 4     | `guide-box-spacing` | Space between legend and panel |
| 5     | `patch-title-left`  | Left patch title               |
| 6     | `ylab-left`         | Left y-axis label              |
| 7     | `axis-left`         | Left axis ticks and labels     |
| 8     | `strip-left`        | Left strip (facet label)       |
| 9     | `panel`             | Main panel area                |
| 10    | `strip-right`       | Right strip (facet label)      |
| 11    | `axis-right`        | Right axis ticks and labels    |
| 12    | `ylab-right`        | Right y-axis label             |
| 13    | `patch-title-right` | Right patch title              |
| 14    | `guide-box-spacing` | Space before right legend box  |
| 15    | `guide-box-right`   | Right legend box               |
| 16    | `tag-right`         | Optional tag on right side     |
| 17    | `margin-right`      | External right spacing         |
