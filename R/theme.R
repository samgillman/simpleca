# R/theme.R
# SimpleCa²⁺ Theme System - Design Tokens & Styling

# ==================== Color Palette ====================

# Primary Colors
primary_blue <- "#0072B2" # Brand accent, buttons, active states
primary_light <- "#E8F4F8" # Subtle backgrounds, hover states
primary_dark <- "#004D7A" # Headers, emphasis

# Neutral Grays
white <- "#FFFFFF" # Main background
gray_50 <- "#F8F9FA" # Subtle backgrounds, alternating rows
gray_100 <- "#E9ECEF" # Borders, dividers
gray_600 <- "#6C757D" # Secondary text
gray_900 <- "#212529" # Primary text

# Semantic Colors
success <- "#28A745" # Validation, positive feedback
warning <- "#FFC107" # Cautions, processing states
danger <- "#DC3545" # Errors, critical warnings
info <- "#17A2B8" # Helpful hints, tooltips

# ==================== Typography ====================

# Font Stacks
font_primary <- "-apple-system, BlinkMacSystemFont, 'Segoe UI', Roboto, 'Helvetica Neue', Arial, sans-serif"
font_mono <- "'SF Mono', Monaco, 'Cascadia Code', 'Courier New', monospace"

# Type Scale (px)
type_hero <- "24px" # Page titles
type_h1 <- "20px" # Section headers
type_h2 <- "16px" # Subsection headers
type_body <- "14px" # Standard text, controls
type_small <- "12px" # Help text, captions
type_tiny <- "11px" # Axis labels, dense tables

# Font Weights
weight_regular <- 400
weight_medium <- 500
weight_semibold <- 600

# ==================== Spacing System ====================

# 4px base unit
spacing_xs <- "4px"
spacing_sm <- "8px"
spacing_md <- "16px"
spacing_lg <- "24px"
spacing_xl <- "32px"
spacing_2xl <- "48px"

# ==================== Elevation & Shadows ====================

shadow_none <- "none"
shadow_level_1 <- "0 1px 3px rgba(0,0,0,0.12)" # Cards, boxes
shadow_level_2 <- "0 4px 6px rgba(0,0,0,0.1)" # Dropdowns, popovers
shadow_level_3 <- "0 10px 20px rgba(0,0,0,0.15)" # Modals, overlays

# ==================== Border Radius ====================

radius_sm <- "3px" # Buttons, inputs
radius_md <- "6px" # Cards, boxes
radius_lg <- "8px" # Modals

# ==================== Helper Functions ====================

#' Generate CSS custom properties
#'
#' Creates a CSS string with all design tokens as custom properties
#' @return Character string containing CSS :root block
get_theme_css_vars <- function() {
  glue::glue("
    :root {{
      /* Colors - Primary */
      --color-primary-blue: {primary_blue};
      --color-primary-light: {primary_light};
      --color-primary-dark: {primary_dark};

      /* Colors - Grays */
      --color-white: {white};
      --color-gray-50: {gray_50};
      --color-gray-100: {gray_100};
      --color-gray-600: {gray_600};
      --color-gray-900: {gray_900};

      /* Colors - Semantic */
      --color-success: {success};
      --color-warning: {warning};
      --color-danger: {danger};
      --color-info: {info};

      /* Typography */
      --font-primary: {font_primary};
      --font-mono: {font_mono};
      --type-hero: {type_hero};
      --type-h1: {type_h1};
      --type-h2: {type_h2};
      --type-body: {type_body};
      --type-small: {type_small};
      --type-tiny: {type_tiny};

      /* Spacing */
      --spacing-xs: {spacing_xs};
      --spacing-sm: {spacing_sm};
      --spacing-md: {spacing_md};
      --spacing-lg: {spacing_lg};
      --spacing-xl: {spacing_xl};
      --spacing-2xl: {spacing_2xl};

      /* Shadows */
      --shadow-level-1: {shadow_level_1};
      --shadow-level-2: {shadow_level_2};
      --shadow-level-3: {shadow_level_3};

      /* Border Radius */
      --radius-sm: {radius_sm};
      --radius-md: {radius_md};
      --radius-lg: {radius_lg};
    }}
  ")
}

#' Generate unified theme CSS
#'
#' Creates complete CSS for consistent styling across the app
#' @return Character string containing all theme CSS
get_unified_theme_css <- function() {
  css_vars <- get_theme_css_vars()

  glue::glue("
    {css_vars}

    /* ==================== Global Styles ==================== */

    body {{
      font-family: var(--font-primary);
      font-size: var(--type-body);
      color: var(--color-gray-900);
      background-color: var(--color-gray-50);
    }}

    /* ==================== Typography ==================== */

    h1, .h1 {{
      font-size: var(--type-h1);
      font-weight: {weight_semibold};
      color: var(--color-gray-900);
      margin-bottom: var(--spacing-md);
    }}

    h2, .h2 {{
      font-size: var(--type-h2);
      font-weight: {weight_semibold};
      color: var(--color-gray-900);
      margin-bottom: var(--spacing-sm);
    }}

    h3, h4, h5, h6 {{
      font-weight: {weight_medium};
      color: var(--color-gray-900);
    }}

    /* ==================== Boxes & Containers ==================== */

    .box {{
      background: var(--color-white);
      border: 1px solid var(--color-gray-100);
      border-radius: var(--radius-md);
      box-shadow: var(--shadow-level-1);
      margin-bottom: 12px;
    }}

    .box-header {{
      background: var(--color-primary-blue);
      color: var(--color-white);
      font-weight: {weight_semibold};
      font-size: var(--type-body);
      padding: 10px 14px;
      border-radius: var(--radius-md) var(--radius-md) 0 0;
    }}

    .box-header .box-title {{
      font-size: var(--type-body) !important;
    }}

    /* Ensure icons in box headers are properly spaced */
    .box-header .fa, .box-header .fas, .box-header .far {{
      margin-right: 6px;
    }}

    .box-body {{
      padding: 12px;
      overflow-x: hidden;
    }}

    /* Override shinydashboard box colors for consistency */
    .box.box-solid.box-primary > .box-header {{
      background: var(--color-primary-blue) !important;
      color: var(--color-white) !important;
    }}

    .box.box-primary > .box-header {{
      background: var(--color-primary-blue) !important;
      color: var(--color-white) !important;
    }}

    /* Ensure box titles are white text */
    .box-header > .box-title {{
      color: var(--color-white) !important;
    }}

    .box.box-solid.box-primary > .box-header > .box-title,
    .box.box-primary > .box-header > .box-title {{
      color: var(--color-white) !important;
    }}

    /* Info status boxes should also use our primary blue for consistency */
    .box.box-solid.box-info > .box-header,
    .box.box-info > .box-header {{
      background: var(--color-primary-blue) !important;
      color: var(--color-white) !important;
    }}

    .box.box-solid.box-info > .box-header > .box-title,
    .box.box-info > .box-header > .box-title {{
      color: var(--color-white) !important;
    }}

    /* ==================== Buttons ==================== */

    .btn {{
      font-weight: {weight_medium};
      border-radius: var(--radius-sm);
      padding: var(--spacing-sm) var(--spacing-md);
      transition: all 150ms ease-in-out;
    }}

    .btn-primary {{
      background-color: var(--color-primary-blue);
      border-color: var(--color-primary-blue);
      color: var(--color-white);
    }}

    .btn-primary:hover {{
      background-color: var(--color-primary-dark);
      border-color: var(--color-primary-dark);
    }}

    .btn-default {{
      background-color: var(--color-white);
      border-color: var(--color-primary-blue);
      color: var(--color-primary-blue);
    }}

    .btn-default:hover {{
      background-color: var(--color-primary-light);
      border-color: var(--color-primary-blue);
    }}

    /* ==================== Form Controls ==================== */

    .form-group {{
      margin-bottom: 12px;
    }}

    .form-group label {{
      font-size: var(--type-small);
      font-weight: {weight_medium};
      color: var(--color-gray-900);
      margin-bottom: 2px;
    }}

    .form-control {{
      border: 1px solid var(--color-gray-100);
      border-radius: var(--radius-sm);
      padding: 6px 12px;
      font-size: var(--type-body);
    }}

    .form-control:focus {{
      border-color: var(--color-primary-blue);
      box-shadow: 0 0 0 2px rgba(0, 114, 178, 0.1);
      outline: none;
    }}

    select.form-control {{
      appearance: none;
      background-image: url('data:image/svg+xml;charset=UTF-8,%3csvg xmlns=\"http://www.w3.org/2000/svg\" viewBox=\"0 0 24 24\" fill=\"none\" stroke=\"currentColor\" stroke-width=\"2\" stroke-linecap=\"round\" stroke-linejoin=\"round\"%3e%3cpolyline points=\"6 9 12 15 18 9\"%3e%3c/polyline%3e%3c/svg%3e');
      background-repeat: no-repeat;
      background-position: right 8px center;
      background-size: 16px;
      padding-right: 32px;
    }}

    /* File Input Styling - Shiny specific */
    .form-group.shiny-input-container {{
      margin-bottom: var(--spacing-md);
    }}

    /* Style the input group for file inputs */
    .input-group {{
      display: flex;
      align-items: stretch;
      width: 100%;
      position: relative;
    }}

    /* Fix the input-group-btn label container that wraps the file button */
    .input-group-btn,
    .input-group-prepend {{
      display: flex !important;
      flex-shrink: 0;
      width: auto !important;
      min-width: fit-content !important;
      overflow: visible !important;
    }}

    /* Style the file input button (Shiny wraps it in a span with btn-file class) */
    .btn-file {{
      background-color: var(--color-primary-blue) !important;
      border-color: var(--color-primary-blue) !important;
      color: var(--color-white) !important;
      border-radius: var(--radius-sm) 0 0 var(--radius-sm);
      padding: 6px 16px !important;
      font-size: var(--type-body);
      font-weight: {weight_medium};
      cursor: pointer !important;
      transition: background-color 150ms ease;
      white-space: nowrap;
      position: relative;
      display: inline-flex !important;
      align-items: center;
      min-height: 34px;
      min-width: 80px !important;
      overflow: visible !important;
    }}

    .btn-file:hover {{
      background-color: var(--color-primary-dark) !important;
      border-color: var(--color-primary-dark) !important;
    }}

    /* Override Shiny's default file input positioning that moves it off-screen */
    /* This is critical - Shiny adds inline styles that position the input at -99999px */
    .btn-file input[type='file'] {{
      position: absolute !important;
      top: 0 !important;
      left: 0 !important;
      right: 0 !important;
      bottom: 0 !important;
      width: 100% !important;
      height: 100% !important;
      opacity: 0 !important;
      cursor: pointer !important;
      z-index: 10 !important;
    }}

    /* Style the text input showing filename */
    .input-group input[type='text'][readonly] {{
      background-color: var(--color-gray-50);
      border: 1px solid var(--color-gray-100);
      border-left: none;
      border-radius: 0 var(--radius-sm) var(--radius-sm) 0;
      padding: 6px 12px;
      font-size: var(--type-body);
      color: var(--color-gray-600);
      flex: 1;
    }}

    /* Remove margin between button and text input */
    .input-group .btn-file + input[type='text'] {{
      margin-left: 0 !important;
    }}

    /* ==================== Sidebar ==================== */

    .main-sidebar {{
      background: var(--color-white) !important;
      width: 230px !important;
      /* Keep menu content below fixed header height so top tabs are not hidden */
      padding-top: 50px !important;
      margin-top: 0 !important;
      overflow-y: auto;
    }}

    .sidebar {{
      background: var(--color-white) !important;
      border-right: 1px solid var(--color-gray-100);
      padding-bottom: 0 !important;
      padding-top: 0 !important;
      margin-top: 0 !important;
      height: 100%;
    }}

    /* Keep menu flush while preserving visibility below header offset */
    .sidebar-menu {{
      padding-top: 0 !important;
      margin-top: 0 !important;
    }}

    .sidebar-menu > li > a {{
      color: var(--color-gray-900) !important;
      font-weight: {weight_regular};
      font-size: 13px !important;
      padding: 9px 16px !important;
      border-left: 3px solid transparent !important;
      transition: all 150ms ease;
    }}

    .sidebar-menu > li > a > .fa, .sidebar-menu > li > a > .fas, .sidebar-menu > li > a > .far {{
      width: 20px !important;
      text-align: center;
      margin-right: 8px;
      font-size: 13px;
      color: var(--color-gray-600);
    }}

    .sidebar-menu > li.active > a {{
      background-color: var(--color-primary-light) !important;
      color: var(--color-primary-dark) !important;
      font-weight: {weight_medium};
      border-left: 3px solid var(--color-primary-blue) !important;
    }}

    .sidebar-menu > li.active > a > .fa,
    .sidebar-menu > li.active > a > .fas,
    .sidebar-menu > li.active > a > .far {{
      color: var(--color-primary-blue) !important;
    }}

    .sidebar-menu > li > a:hover {{
      background-color: var(--color-gray-50) !important;
      color: var(--color-gray-900) !important;
    }}

    /* Sidebar section headers (PRE-NORMALIZED DATA, NORMALIZED DATA, GENERAL) */
    .sidebar-menu > li.header {{
      background: transparent !important;
      color: var(--color-gray-600) !important;
      font-size: 10px !important;
      font-weight: {weight_semibold} !important;
      letter-spacing: 0.8px !important;
      text-transform: uppercase !important;
      padding: 14px 16px 6px 16px !important;
      border-bottom: none !important;
      line-height: 1.2 !important;
    }}

    /* First section header needs less top padding */
    .sidebar-menu > li.header:first-child {{
      padding-top: 10px !important;
    }}

    /* Remove any sidebar footer or extra elements */
    .sidebar-footer {{
      display: none !important;
    }}

    /* ==================== Dashboard Header ==================== */

    .main-header .logo {{
      background-color: var(--color-primary-blue) !important;
      color: var(--color-white) !important;
      font-weight: {weight_semibold};
      font-size: 18px;
      letter-spacing: 0.3px;
      width: 230px !important;
      max-width: 230px !important;
      min-width: 230px !important;
      text-align: center !important;
      padding: 0 !important;
      margin: 0 !important;
      height: 50px !important;
      line-height: 50px !important;
      display: flex !important;
      align-items: center !important;
      justify-content: center !important;
      box-sizing: border-box !important;
    }}

    .main-header .navbar {{
      background-color: var(--color-primary-blue) !important;
      margin-left: 230px !important;
      min-height: 50px !important;
    }}

    .sidebar-toggle {{
      display: block !important;
    }}

    /* ==================== Content & Layout Alignment ==================== */

    .content-wrapper {{
      background-color: var(--color-gray-50);
      margin-left: 230px !important;
    }}

    .content {{
      padding: 16px;
    }}

    /* Keep sidebar collapse behavior functional */
    body.sidebar-collapse .main-header .logo {{
      width: 50px !important;
      min-width: 50px !important;
      max-width: 50px !important;
      overflow: hidden;
      font-size: 0 !important;
    }}

    body.sidebar-collapse .main-header .navbar,
    body.sidebar-collapse .content-wrapper,
    body.sidebar-collapse .main-footer {{
      margin-left: 50px !important;
    }}

    /* Mobile/tablet: let layout stack and preserve off-canvas sidebar behavior */
    @media (max-width: 991px) {{
      .main-sidebar {{
        padding-top: 50px !important;
      }}

      .main-header .logo {{
        width: 100% !important;
        min-width: 0 !important;
        max-width: none !important;
        float: none !important;
      }}

      .main-header .navbar,
      .content-wrapper,
      .main-footer {{
        margin-left: 0 !important;
      }}

      .content {{
        padding: 12px;
      }}
    }}

    /* Ensure fluidRow has no weird margins */
    .row {{
      margin-left: 0;
      margin-right: 0;
    }}

    /* Tighter column gutters for compact layout */
    [class*='col-'] {{
      padding-left: 8px;
      padding-right: 8px;
    }}

    /* Stat card refinements */
    .stat-card {{
      transition: box-shadow 150ms ease;
    }}

    .stat-card:hover {{
      box-shadow: var(--shadow-level-2);
    }}

    /* ==================== Tables ==================== */

    table {{
      font-size: var(--type-body);
    }}

    table th {{
      font-weight: {weight_semibold};
      background-color: var(--color-gray-50);
      border-bottom: 2px solid var(--color-gray-100);
    }}

    table td {{
      border-bottom: 1px solid var(--color-gray-100);
    }}

    /* DataTables (DT) styling */
    .dataTables_wrapper {{
      font-size: var(--type-small);
    }}

    .dataTables_wrapper .dataTables_filter input {{
      border: 1px solid var(--color-gray-100);
      border-radius: var(--radius-sm);
      padding: 4px 8px;
      font-size: var(--type-small);
    }}

    .dataTables_wrapper .dataTables_length select {{
      border: 1px solid var(--color-gray-100);
      border-radius: var(--radius-sm);
      padding: 2px 6px;
      font-size: var(--type-small);
    }}

    /* DT header cells */
    table.dataTable thead th {{
      font-weight: {weight_semibold};
      background-color: var(--color-gray-50);
      border-bottom: 2px solid var(--color-primary-blue) !important;
      color: var(--color-gray-900);
      padding: 10px 8px;
      font-size: var(--type-small);
    }}

    /* DT body rows */
    table.dataTable tbody td {{
      padding: 8px;
      vertical-align: middle;
      border-bottom: 1px solid var(--color-gray-100);
    }}

    table.dataTable tbody tr:hover {{
      background-color: var(--color-primary-light) !important;
    }}

    /* DT stripe rows */
    table.dataTable.stripe tbody tr.odd,
    table.dataTable.display tbody tr.odd {{
      background-color: rgba(0, 0, 0, 0.015);
    }}

    /* DT pagination */
    .dataTables_wrapper .dataTables_paginate .paginate_button {{
      border: 1px solid var(--color-gray-100) !important;
      border-radius: var(--radius-sm);
      padding: 4px 10px !important;
      margin: 0 2px;
      font-size: var(--type-small);
    }}

    .dataTables_wrapper .dataTables_paginate .paginate_button.current {{
      background: var(--color-primary-blue) !important;
      border-color: var(--color-primary-blue) !important;
      color: var(--color-white) !important;
    }}

    .dataTables_wrapper .dataTables_paginate .paginate_button:hover {{
      background: var(--color-primary-light) !important;
      border-color: var(--color-primary-blue) !important;
      color: var(--color-primary-dark) !important;
    }}

    /* DT info text */
    .dataTables_wrapper .dataTables_info {{
      font-size: var(--type-tiny);
      color: var(--color-gray-600);
      padding-top: 10px;
    }}

    /* DT top filter inputs */
    table.dataTable thead .sorting_asc::after,
    table.dataTable thead .sorting_desc::after,
    table.dataTable thead .sorting::after {{
      opacity: 0.4;
    }}

    /* DT column filters (filter = 'top') */
    .dataTables_wrapper thead input[type=search],
    .dataTables_wrapper thead select {{
      font-size: var(--type-tiny);
      padding: 2px 4px;
      border: 1px solid var(--color-gray-100);
      border-radius: var(--radius-sm);
      width: 100%;
      margin-top: 4px;
    }}

    /* DT Buttons extension */
    .dataTables_wrapper .dt-buttons {{
      margin-bottom: 10px;
    }}

    .dataTables_wrapper .dt-buttons .btn {{
      font-size: var(--type-tiny) !important;
      padding: 4px 10px !important;
    }}

    /* Metrics explanation styling */
    .metric-data-box {{
      background-color: var(--color-gray-50);
      padding: 15px;
      border-radius: var(--radius-sm);
      border-left: 4px solid var(--color-primary-blue);
    }}

    .metric-data-box ul {{
      margin-bottom: 0;
    }}

    .metric-result-box {{
      background-color: #d4edda;
      padding: 10px;
      border-radius: var(--radius-sm);
      margin-top: 10px;
      border: 1px solid #c3e6cb;
    }}

    .metric-result-box h5 {{
      margin: 0;
      color: #155724;
    }}

    .metric-result-box p {{
      margin: 5px 0 0 0;
      font-weight: {weight_semibold};
      color: #155724;
    }}

    .metric-section-header {{
      color: var(--color-gray-900);
      border-bottom: 2px solid var(--color-primary-blue);
      padding-bottom: 5px;
    }}

    .metric-note {{
      font-style: italic;
      color: #856404;
    }}

    /* Tab pills styling */
    .nav-pills > li > a {{
      border-radius: var(--radius-sm);
      color: var(--color-gray-600);
      font-size: var(--type-small);
      padding: 8px 14px;
      margin-right: 4px;
    }}

    .nav-pills > li.active > a,
    .nav-pills > li.active > a:hover,
    .nav-pills > li.active > a:focus {{
      background-color: var(--color-primary-blue);
      color: var(--color-white);
    }}

    .nav-pills > li > a:hover {{
      background-color: var(--color-primary-light);
      color: var(--color-primary-dark);
    }}

    /* ==================== Loading Spinners ==================== */

    .spinner {{
      color: var(--color-primary-blue);
    }}

    /* ==================== Utility Classes ==================== */

    .text-muted {{
      color: var(--color-gray-600);
    }}

    .text-primary {{
      color: var(--color-primary-blue);
    }}

    .bg-light {{
      background-color: var(--color-gray-50);
    }}

    .border {{
      border: 1px solid var(--color-gray-100);
    }}

    .rounded {{
      border-radius: var(--radius-md);
    }}

    .shadow {{
      box-shadow: var(--shadow-level-1);
    }}

    /* ==================== Accordion Component ==================== */

    .accordion-section {{
      margin-bottom: 10px;
      border: 1px solid var(--color-gray-100);
      border-radius: var(--radius-md);
      background: var(--color-white);
      /* Don't use overflow hidden - it clips dropdown menus */
    }}

    .accordion-header {{
      padding: 8px 12px;
      background: var(--color-gray-50);
      cursor: pointer;
      user-select: none;
      display: flex;
      align-items: center;
      gap: var(--spacing-sm);
      transition: background-color 150ms ease;
      font-weight: {weight_medium};
      font-size: 13px;
      color: var(--color-gray-900);
      border-radius: var(--radius-md) var(--radius-md) 0 0;
    }}

    .accordion-header:hover {{
      background: var(--color-primary-light);
    }}

    .accordion-header:active {{
      background: var(--color-gray-100);
    }}

    .accordion-title {{
      flex: 1;
      font-size: var(--type-body);
    }}

    .accordion-chevron {{
      transition: transform 200ms ease;
      color: var(--color-gray-600);
      font-size: 12px;
    }}

    .accordion-body {{
      transition: max-height 300ms ease, padding 300ms ease;
      overflow: hidden;
    }}

    .accordion-body.collapsed {{
      max-height: 0 !important;
      padding: 0 !important;
      overflow: hidden;
    }}

    .accordion-body.expanded {{
      max-height: 2000px;
      padding: 12px;
      overflow: visible;
    }}

    .accordion-content {{
      /* Content wrapper for additional spacing/styling */
    }}

    /* Nested input styling within accordions */
    .accordion-content .form-group {{
      margin-bottom: 6px;
    }}

    .accordion-content .form-group:last-child {{
      margin-bottom: 0;
    }}

    /* Ensure select dropdowns in accordions appear above other content */
    .accordion-content .selectize-dropdown,
    .accordion-content select {{
      z-index: 1000;
    }}

    .accordion-content .form-group {{
      position: relative;
      z-index: auto;
    }}

    /* Text overflow protection */
    .small-help, .box-body p, .text-muted.small {{
      overflow-wrap: break-word;
      word-wrap: break-word;
    }}

    /* Shiny selectInput dropdown positioning */
    .selectize-dropdown {{
      z-index: 1050 !important;
    }}

    /* ==================== Status Steps ==================== */

    .status-step {{
      text-align: center;
      padding: 10px 4px;
    }}

    .status-step-icon {{
      font-size: 20px;
      margin-bottom: 6px;
      transition: color 0.3s ease, transform 0.3s ease;
    }}

    .status-step-label {{
      font-size: 11px;
      font-weight: {weight_semibold};
      color: var(--color-gray-900);
      margin-bottom: 2px;
      text-transform: uppercase;
      letter-spacing: 0.3px;
    }}

    .status-step-text {{
      font-size: 11px;
      color: var(--color-gray-600);
      line-height: 1.3;
    }}

    /* ==================== Enhanced Stat Cards ==================== */

    .stat-card {{
      transition: transform 150ms ease, box-shadow 150ms ease;
    }}

    .stat-card:hover {{
      transform: translateY(-1px);
      box-shadow: var(--shadow-level-2);
    }}

    /* ==================== Enhanced Buttons ==================== */

    .btn-primary {{
      box-shadow: 0 1px 3px rgba(0, 114, 178, 0.25);
    }}

    .btn-primary:hover {{
      box-shadow: 0 2px 6px rgba(0, 114, 178, 0.35);
      transform: translateY(-1px);
    }}

    .btn-primary:active {{
      transform: translateY(0);
      box-shadow: 0 1px 2px rgba(0, 114, 178, 0.2);
    }}

    .btn-default {{
      box-shadow: 0 1px 2px rgba(0, 0, 0, 0.05);
    }}

    .btn-default:hover {{
      box-shadow: 0 2px 4px rgba(0, 0, 0, 0.08);
      transform: translateY(-1px);
    }}

    /* Download buttons — subtle secondary style */
    .btn.btn-primary[id*=\"dl_\"],
    .btn.btn-primary[id*=\"download\"] {{
      background-color: var(--color-primary-blue);
    }}

    /* ==================== Enhanced Accordions ==================== */

    .accordion-section {{
      transition: box-shadow 150ms ease;
    }}

    .accordion-section:hover {{
      box-shadow: 0 1px 4px rgba(0, 0, 0, 0.06);
    }}

    .accordion-header {{
      border-radius: var(--radius-md);
    }}

    .accordion-body.expanded + .accordion-header,
    .accordion-section:has(.accordion-body.expanded) > .accordion-header {{
      border-radius: var(--radius-md) var(--radius-md) 0 0;
    }}

    /* ==================== Enhanced Empty States ==================== */

    .empty-state-container {{
      text-align: center;
      padding: 48px 24px;
    }}

    .empty-state-container .fa {{
      color: var(--color-primary-blue);
      opacity: 0.15;
      margin-bottom: 16px;
      font-size: 48px;
      display: block;
    }}

    .empty-state-container h4 {{
      font-weight: {weight_semibold};
      color: var(--color-gray-900);
      margin: 0 0 8px 0;
    }}

    .empty-state-container p {{
      color: var(--color-gray-600);
      font-size: 13px;
      margin: 0;
      max-width: 320px;
      margin-left: auto;
      margin-right: auto;
    }}

    /* ==================== Logo in Header ==================== */

    .main-header .logo img {{
      filter: brightness(0) invert(1);
    }}
  ")
}

#' Get the JavaScript for accordion functionality and file input fix
#'
#' @return Character string with accordion JavaScript and file input fix
get_accordion_js <- function() {
  HTML("
  <script>
  function toggleAccordion(id) {
    // Find the accordion elements
    const body = document.querySelector(`[data-accordion-id='${id}'] .accordion-body`);
    const chevron = document.querySelector(`[data-accordion-id='${id}'] .accordion-chevron`);

    if (!body || !chevron) return;

    // Toggle collapsed/expanded classes
    const isCollapsed = body.classList.contains('collapsed');

    if (isCollapsed) {
      // Expand
      body.classList.remove('collapsed');
      body.classList.add('expanded');
      chevron.style.transform = 'rotate(180deg)';
    } else {
      // Collapse
      body.classList.remove('expanded');
      body.classList.add('collapsed');
      chevron.style.transform = 'rotate(0deg)';
    }
  }

  // Fix Shiny file input positioning
  // Shiny adds inline styles with !important that position file inputs off-screen
  // We need to override these inline styles with JavaScript
  function fixFileInputs() {
    const fileInputs = document.querySelectorAll('input[type=\"file\"]');
    fileInputs.forEach(function(input) {
      // Remove the problematic inline positioning
      input.style.setProperty('position', 'absolute', 'important');
      input.style.setProperty('top', '0', 'important');
      input.style.setProperty('left', '0', 'important');
      input.style.setProperty('right', '0', 'important');
      input.style.setProperty('bottom', '0', 'important');
      input.style.setProperty('width', '100%', 'important');
      input.style.setProperty('height', '100%', 'important');
      input.style.setProperty('opacity', '0', 'important');
      input.style.setProperty('cursor', 'pointer', 'important');
      input.style.setProperty('z-index', '10', 'important');
    });
  }

  let fileInputObserver = null;

  function setupFileInputObserver() {
    if (!window.MutationObserver || !document.body) return;
    if (fileInputObserver) fileInputObserver.disconnect();

    fileInputObserver = new MutationObserver(function(mutations) {
      for (const mutation of mutations) {
        if (mutation.type === 'childList' && (mutation.addedNodes.length > 0 || mutation.removedNodes.length > 0)) {
          window.requestAnimationFrame(fixFileInputs);
          break;
        }
      }
    });

    fileInputObserver.observe(document.body, { childList: true, subtree: true });
  }

  // Run once on load and when Shiny reconnects/updates DOM.
  document.addEventListener('DOMContentLoaded', function() {
    fixFileInputs();
    setupFileInputObserver();
  });

  $(document).on('shiny:connected', function() {
    fixFileInputs();
    setupFileInputObserver();
  });

  $(document).on('shiny:value', function() {
    window.requestAnimationFrame(fixFileInputs);
  });
  </script>
  ")
}

# ==================== ggplot Theme Builder ====================

#' Build a consistent ggplot theme from standard inputs
#'
#' This is the ONLY function that should be used to create ggplot themes.
#' All modules MUST use this function to ensure visual consistency.
#' No module should create its own theme logic.
#'
#' @param theme_name One of "classic", "minimal", "light", "dark"
#' @param title_size Title font size (default: 18)
#' @param bold_title Logical, bold title (default: TRUE)
#' @param axis_title_size Axis title font size (default: 14)
#' @param bold_axis_title Logical, bold axis titles (default: TRUE)
#' @param axis_size Axis text size (default: 12)
#' @param bold_axis_text Logical, bold axis text (default: FALSE)
#' @param font Font family name (default: "Arial")
#' @param legend_pos Legend position - "none", "bottom", "right", "top", "left" (default: "none")
#' @param grid_major Show major gridlines (default: FALSE)
#' @param grid_minor Show minor gridlines (default: FALSE)
#'
#' @return A ggplot2 theme object
#' @export
build_plot_theme <- function(
  theme_name = "classic",
  title_size = 18,
  bold_title = TRUE,
  axis_title_size = 14,
  bold_axis_title = TRUE,
  axis_size = 12,
  bold_axis_text = FALSE,
  font = "Arial",
  legend_pos = "none",
  grid_major = FALSE,
  grid_minor = FALSE
) {
  # Base theme selection
  base_theme <- switch(theme_name %||% "classic",
    classic = ggplot2::theme_classic(),
    minimal = ggplot2::theme_minimal(),
    light = ggplot2::theme_light(),
    dark = ggplot2::theme_dark(),
    ggplot2::theme_classic()
  )

  # Build customized theme
  theme_result <- base_theme + ggplot2::theme(
    # Title
    plot.title = ggplot2::element_text(
      hjust = 0.5,
      size = title_size %||% 18,
      face = if (isTRUE(bold_title)) "bold" else "plain",
      family = font %||% "Arial"
    ),
    # Subtitle
    plot.subtitle = ggplot2::element_text(
      hjust = 0.5,
      size = max(8, (title_size %||% 18) - 4),
      family = font %||% "Arial"
    ),
    # Axis titles
    axis.title = ggplot2::element_text(
      size = axis_title_size %||% 14,
      face = if (isTRUE(bold_axis_title)) "bold" else "plain",
      family = font %||% "Arial"
    ),
    # Axis text
    axis.text = ggplot2::element_text(
      size = axis_size %||% 12,
      face = if (isTRUE(bold_axis_text)) "bold" else "plain",
      family = font %||% "Arial"
    ),
    # Legend
    legend.position = legend_pos %||% "none",
    legend.title = ggplot2::element_text(
      face = "bold",
      size = (axis_title_size %||% 14) * 0.85
    ),
    legend.text = ggplot2::element_text(
      size = (axis_size %||% 12) * 0.9
    ),
    # Margins
    plot.margin = ggplot2::margin(10, 25, 10, 10)
  )

  # Gridlines
  if (isTRUE(grid_major) || isTRUE(grid_minor)) {
    theme_result <- theme_result + ggplot2::theme(
      panel.grid.major = if (isTRUE(grid_major)) {
        ggplot2::element_line(color = "grey90", linewidth = 0.3)
      } else {
        ggplot2::element_blank()
      },
      panel.grid.minor = if (isTRUE(grid_minor)) {
        ggplot2::element_line(color = "grey95", linewidth = 0.2)
      } else {
        ggplot2::element_blank()
      }
    )
  } else {
    theme_result <- theme_result + ggplot2::theme(
      panel.grid = ggplot2::element_blank()
    )
  }

  return(theme_result)
}

#' Get theme parameters from Shiny inputs
#'
#' Helper function to extract theme parameters from Shiny input object.
#' Ensures consistent parameter extraction across all modules.
#'
#' @param input Shiny input object
#' @param prefix Input ID prefix (e.g., "tc", "pa", "metric")
#'
#' @return A list of theme parameters suitable for build_plot_theme()
get_theme_params_from_input <- function(input, prefix = "tc") {
  # Support consolidated typography: base_font_size + bold_labels
  # Derives individual sizes: title = base+4, axis_title = base, axis_text = max(8, base-2)
  base_size <- input[[paste0(prefix, "_base_font_size")]]
  bold_labels <- input[[paste0(prefix, "_bold_labels")]]

  if (!is.null(base_size)) {
    # New consolidated pattern
    bold <- isTRUE(bold_labels %||% TRUE)
    list(
      theme_name = input[[paste0(prefix, "_theme")]] %||% "classic",
      title_size = base_size + 4,
      bold_title = bold,
      axis_title_size = base_size,
      bold_axis_title = bold,
      axis_size = max(8, base_size - 2),
      bold_axis_text = FALSE,
      font = input[[paste0(prefix, "_font")]] %||% "Arial",
      legend_pos = input[[paste0(prefix, "_legend_pos")]] %||% "none",
      grid_major = FALSE,
      grid_minor = FALSE
    )
  } else {
    # Legacy individual controls pattern
    list(
      theme_name = input[[paste0(prefix, "_theme")]] %||% "classic",
      title_size = input[[paste0(prefix, "_title_size")]] %||% 18,
      bold_title = input[[paste0(prefix, "_bold_title")]] %||% TRUE,
      axis_title_size = input[[paste0(prefix, "_axis_title_size")]] %||% 14,
      bold_axis_title = input[[paste0(prefix, "_bold_axis_title")]] %||% TRUE,
      axis_size = input[[paste0(prefix, "_axis_size")]] %||% 12,
      bold_axis_text = input[[paste0(prefix, "_bold_axis_text")]] %||% FALSE,
      font = input[[paste0(prefix, "_font")]] %||% "Arial",
      legend_pos = input[[paste0(prefix, "_legend_pos")]] %||% "none",
      grid_major = input[[paste0(prefix, "_grid_major")]] %||% FALSE,
      grid_minor = input[[paste0(prefix, "_grid_minor")]] %||% FALSE
    )
  }
}

#' Build plot theme from Shiny inputs
#'
#' Convenience function that combines get_theme_params_from_input and build_plot_theme.
#' This is the recommended way to get a theme in module server code.
#'
#' @param input Shiny input object
#' @param prefix Input ID prefix (e.g., "tc", "pa", "metric")
#'
#' @return A ggplot2 theme object
build_plot_theme_from_input <- function(input, prefix = "tc") {
  params <- get_theme_params_from_input(input, prefix)
  do.call(build_plot_theme, params)
}
