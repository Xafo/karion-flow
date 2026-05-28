---
title: Karion Flow
emoji: 🔬
colorFrom: blue
colorTo: green
sdk: docker
pinned: false
---

# Karion Flow

Pipeline de análisis de citometría de flujo (CLSI H62) para archivos FCS.

## Endpoints

- `POST /api/analizar` — Subir archivos FCS (JSON/base64) e iniciar análisis
- `GET /api/estado/:id` — Estado del análisis
- `GET /api/reporte/:id` — Reporte HTML autocontenido
- `GET /api/gates/:id` — Datos de poblaciones detectadas
- `GET /api/widget3d/:id` — Widget 3D interactivo
- `GET /api/template` — Template de gating actual
- `POST /api/template` — Actualizar template de gating
- `GET /api/health` — Health check
