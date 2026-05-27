const API_BASE = window.location.hostname === 'localhost' || window.location.hostname === '127.0.0.1'
  ? 'http://localhost:7860'
  : 'https://xafor2-karion-flow.hf.space';

const state = {
  files: [],
  analysisId: null,
  pollingInterval: null,
  historial: JSON.parse(localStorage.getItem('karion_historial') || '[]')
};

const $ = id => document.getElementById(id);
const $$ = sel => document.querySelectorAll(sel);

// ---- Navigation ----
document.querySelectorAll('.app-nav a').forEach(link => {
  link.addEventListener('click', e => {
    e.preventDefault();
    const page = link.dataset.page;
    document.querySelectorAll('.app-nav a').forEach(a => a.classList.remove('active'));
    link.classList.add('active');
    document.querySelectorAll('.page').forEach(p => p.classList.remove('active'));
    $(`page-${page}`).classList.add('active');
  });
});

// ---- Dropzone ----
const dropzone = $('dropzone');
const fileInput = $('file-input');

dropzone.addEventListener('click', () => fileInput.click());

dropzone.addEventListener('dragover', e => {
  e.preventDefault();
  dropzone.classList.add('dragover');
});
dropzone.addEventListener('dragleave', () => dropzone.classList.remove('dragover'));
dropzone.addEventListener('drop', e => {
  e.preventDefault();
  dropzone.classList.remove('dragover');
  handleFiles(e.dataTransfer.files);
});

fileInput.addEventListener('change', () => handleFiles(fileInput.files));

function handleFiles(fileList) {
  const fcsFiles = [];
  for (const f of fileList) {
    if (f.name.toLowerCase().endsWith('.fcs')) {
      fcsFiles.push(f);
    }
  }
  if (fcsFiles.length === 0) {
    alert('Solo se aceptan archivos .fcs');
    return;
  }
  state.files = fcsFiles;
  renderFileList();
}

function renderFileList() {
  const list = $('file-items');
  const container = $('file-list');
  const count = $('file-count');
  const btn = $('btn-analyze');

  if (state.files.length === 0) {
    container.hidden = true;
    return;
  }

  container.hidden = false;
  count.textContent = state.files.length;
  list.innerHTML = state.files.map(f =>
    `<li><span>📄</span>${f.name}<span class="file-size">${formatSize(f.size)}</span></li>`
  ).join('');
  btn.disabled = false;
}

function formatSize(bytes) {
  if (bytes < 1024) return bytes + ' B';
  if (bytes < 1024 * 1024) return (bytes / 1024).toFixed(1) + ' KB';
  return (bytes / (1024 * 1024)).toFixed(1) + ' MB';
}

// ---- Clear ----
$('btn-clear').addEventListener('click', () => {
  state.files = [];
  state.analysisId = null;
  state.pollingInterval = null;
  $('file-list').hidden = true;
  $('progress-card').hidden = true;
  $('results-card').hidden = true;
  $('btn-analyze').disabled = true;
  fileInput.value = '';
});

// ---- Analyze ----
$('btn-analyze').addEventListener('click', startAnalysis);

function readFileAsBase64(file) {
  return new Promise((resolve, reject) => {
    const reader = new FileReader();
    reader.onload = () => {
      const base64 = reader.result.split(',')[1];
      resolve(base64);
    };
    reader.onerror = reject;
    reader.readAsDataURL(file);
  });
}

async function startAnalysis() {
  if (state.files.length === 0) return;

  $('progress-card').hidden = false;
  $('results-card').hidden = true;
  $('progress-fill').style.width = '5%';
  $('progress-status').textContent = 'Codificando archivos...';

  $('progress-fill').style.width = '10%';
  $('progress-status').textContent = 'Subiendo archivos...';

  const filesPayload = [];
  for (const f of state.files) {
    const b64 = await readFileAsBase64(f);
    filesPayload.push({ name: f.name, data: b64 });
  }

  try {
    const resp = await fetch(`${API_BASE}/api/analizar`, {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({ files: filesPayload })
    });
    const data = await resp.json();

    if (!resp.ok || data.error) {
      $('progress-status').textContent = 'Error: ' + (data.error || 'Error de conexión');
      $('progress-fill').style.width = '0%';
      return;
    }

    state.analysisId = data.id;
    $('progress-id').textContent = `ID: ${data.id}`;
    $('progress-status').textContent = 'Análisis en progreso...';
    $('progress-fill').style.width = '20%';

    // Poll for completion
    pollStatus(data.id);

  } catch (err) {
    $('progress-status').textContent = 'Error de conexión: ' + err.message;
    $('progress-fill').style.width = '0%';
  }
}

async function pollStatus(id) {
  if (state.pollingInterval) clearInterval(state.pollingInterval);

  state.pollingInterval = setInterval(async () => {
    try {
      const resp = await fetch(`${API_BASE}/api/estado/${id}`);
      const data = await resp.json();

      if (data.estado === 'completado') {
        clearInterval(state.pollingInterval);
        state.pollingInterval = null;
        $('progress-fill').style.width = '100%';
        $('progress-status').textContent = 'Análisis completado.';
        setTimeout(() => {
          $('progress-card').hidden = true;
          showResults(id);
        }, 600);
      } else if (data.estado === 'error') {
        clearInterval(state.pollingInterval);
        state.pollingInterval = null;
        $('progress-fill').style.width = '0%';
        $('progress-status').textContent = 'Error: ' + (data.error || 'Error desconocido');
      } else {
        // Still processing
        const pct = Math.min(80, 20 + parseInt(data.progreso || 0) * 0.6);
        $('progress-fill').style.width = pct + '%';
        $('progress-status').textContent = 'Procesando...';
      }
    } catch (err) {
      $('progress-status').textContent = 'Esperando servidor...';
    }
  }, 2000);
}

async function showResults(id) {
  $('results-card').hidden = false;

  $('btn-view-report').href = `${API_BASE}/api/reporte/${id}`;
  $('btn-view-3d').href = `${API_BASE}/api/widget3d/${id}`;

  // Load gates data
  try {
    const resp = await fetch(`${API_BASE}/api/gates/${id}`);
    const data = await resp.json();
    if (data.composicion && data.composicion.length > 0) {
      renderGates(data.composicion);
    }
  } catch (err) {
    // Gates are optional
  }

  // Save to history
  const entry = {
    id,
    date: new Date().toISOString(),
    files: state.files.length
  };
  state.historial.unshift(entry);
  localStorage.setItem('karion_historial', JSON.stringify(state.historial));
  renderHistorial();
}

function renderGates(composicion) {
  const section = $('gates-section');
  const table = $('gates-table');
  section.hidden = false;

  const cols = Object.keys(composicion[0]);
  let html = '<table><thead><tr>';
  for (const col of cols) {
    html += `<th>${col}</th>`;
  }
  html += '</tr></thead><tbody>';
  for (const row of composicion) {
    html += '<tr>';
    for (const col of cols) {
      html += `<td>${row[col]}</td>`;
    }
    html += '</tr>';
  }
  html += '</tbody></table>';
  table.innerHTML = html;
}

// ---- New analysis ----
$('btn-new-analysis').addEventListener('click', () => {
  state.files = [];
  state.analysisId = null;
  $('file-list').hidden = true;
  $('results-card').hidden = true;
  $('btn-analyze').disabled = true;
  fileInput.value = '';
});

// ---- Template ----
$('btn-save-template').addEventListener('click', async () => {
  const umbral = parseFloat($('template-umbral').value);
  const nPob = parseInt($('template-poblaciones').value);

  try {
    const resp = await fetch(`${API_BASE}/api/template`, {
      method: 'POST',
      headers: { 'Content-Type': 'application/json' },
      body: JSON.stringify({ umbral_positividad: umbral, n_poblaciones: nPob })
    });
    const data = await resp.json();
    if (resp.ok) {
      $('template-status').textContent = '✓ Template guardado correctamente';
      setTimeout(() => { $('template-status').textContent = ''; }, 3000);
    } else {
      $('template-status').textContent = 'Error: ' + (data.error || 'Error de conexión');
    }
  } catch (err) {
    $('template-status').textContent = 'Error de conexión: ' + err.message;
  }
});

// ---- Historial ----
function renderHistorial() {
  const list = $('historial-list');
  if (state.historial.length === 0) {
    list.innerHTML = '<p class="empty-state">No hay análisis previos.</p>';
    return;
  }

  list.innerHTML = state.historial.map(item => `
    <div class="historial-item">
      <span class="historial-id">${item.id}</span>
      <span class="historial-date">${new Date(item.date).toLocaleString()}</span>
      <span class="historial-files">${item.files} archivos</span>
      <button class="btn btn-small btn-outline" onclick="loadHistorial('${item.id}')">Ver</button>
    </div>
  `).join('');
}

async function loadHistorial(id) {
  state.analysisId = id;
  showResults(id);
  // Switch to upload page
  document.querySelectorAll('.app-nav a').forEach(a => a.classList.remove('active'));
  document.querySelector('[data-page="upload"]').classList.add('active');
  document.querySelectorAll('.page').forEach(p => p.classList.remove('active'));
  $('page-upload').classList.add('active');
}

// Init
renderHistorial();
