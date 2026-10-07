// Record the demo scenes by driving the real web UI with Playwright.
// Starts the Racket server on maps/bordeaux.osm, types node ids in the forms, submits them
// and scrolls to the SVG map. A caption is overlaid on each page to say what is shown.
// Frames are captured as lossless PNG screenshots with their timestamps (Playwright's own
// video recorder is a low-bitrate VP8 that blurs the thin streets), then demo/record.sh
// turns them into videos. Usage: node demo/record.mjs. Frames go to demo/out/<scene>/.
import { spawn } from 'node:child_process'
import { mkdirSync, rmSync, writeFileSync } from 'node:fs'
import { dirname, join, resolve } from 'node:path'
import { fileURLToPath } from 'node:url'
import { chromium } from 'playwright'

const root = resolve(dirname(fileURLToPath(import.meta.url)), '..')
const out = join(root, 'demo', 'out')
const BASE = 'http://localhost:9000'
const SIZE = { width: 1296, height: 810 }

const P = {
  peyBerland: '7266323279',
  parlement: '1320554415',
  saintPierre: '35258541',
  camilleJullian: '6581917877',
  lafargue: '251542437',
  palais: '35258447',
  vieilleTour: '13125256537',
  quaiBourgeois: '35258525',
  hotelDeVille: '2047703799',
}

const scenes = [
  {
    name: 'overview',
    caption: 'Bordeaux city centre · 1,299 street nodes parsed from OpenStreetMap',
    run: async (page) => {
      await page.goto(BASE)
      await pause(page, 1500)
      await toMap(page)
      await pause(page, 3000)
    },
  },
  {
    name: 'node',
    caption: 'Node page · Place Pey-Berland',
    run: async (page) => {
      await page.goto(`${BASE}/node`)
      await fill(page, 'form[action="/node"]', { node: P.peyBerland })
      await toMap(page)
      await pause(page, 3000)
    },
  },
  {
    name: 'route-short',
    caption: 'Shortest path (Dijkstra) · Place Camille-Jullian → Place Saint-Pierre',
    run: async (page) => {
      await page.goto(`${BASE}/route`)
      await fill(page, 'form[action="/route"]', { start: P.camilleJullian, end: P.saintPierre })
      await showResult(page, '#nb-path')
      await toMap(page)
      await pause(page, 3000)
    },
  },
  {
    name: 'route-long',
    caption: 'Shortest path (Dijkstra) · Rue de la Vieille-Tour → Rue du Quai-Bourgeois, across the centre',
    run: async (page) => {
      await page.goto(`${BASE}/route`)
      await fill(page, 'form[action="/route"]', { start: P.vieilleTour, end: P.quaiBourgeois })
      await showResult(page, '#nb-path')
      await toMap(page)
      await pause(page, 3000)
    },
  },
  {
    name: 'distance',
    caption: 'Distance along the path · Place Pey-Berland → Place du Parlement',
    run: async (page) => {
      await page.goto(`${BASE}/distance`)
      await fill(page, 'form[action="/distance"]', { start: P.peyBerland, end: P.parlement })
      await showResult(page, '#distance')
      await toMap(page)
      await pause(page, 3000)
    },
  },
  {
    name: 'cycle-3',
    caption: 'Travelling salesman (nearest neighbour) · 3 places, back to the start',
    run: async (page) => {
      await page.goto(`${BASE}/cycle`)
      await fill(page, 'form[action="/cycle"]', { nodes: [P.saintPierre, P.lafargue, P.camilleJullian].join(',') })
      await toMap(page)
      await pause(page, 3500)
    },
  },
  {
    name: 'cycle-5',
    caption: 'Travelling salesman (nearest neighbour) · 5 places, no crossing used twice',
    run: async (page) => {
      await page.goto(`${BASE}/cycle`)
      await fill(page, 'form[action="/cycle"]', { nodes: [P.peyBerland, P.saintPierre, P.lafargue, P.palais, P.camilleJullian].join(',') })
      await toMap(page)
      await pause(page, 3500)
    },
  },
  {
    name: 'errors',
    caption: 'Edge cases · same node twice, then a node cut off from the rest of the map',
    run: async (page) => {
      await page.goto(`${BASE}/route`)
      await fill(page, 'form[action="/route"]', { start: P.peyBerland, end: P.peyBerland })
      await pause(page, 2500)
      await page.goto(`${BASE}/route`)
      await fill(page, 'form[action="/route"]', { start: P.peyBerland, end: P.hotelDeVille })
      await pause(page, 3000)
    },
  },
]

const pause = (page, ms) => page.waitForTimeout(ms)

async function caption(page, text) {
  await page.evaluate((text) => {
    const el = document.createElement('div')
    el.textContent = text
    Object.assign(el.style, {
      position: 'fixed', left: '16px', bottom: '16px', zIndex: 10, padding: '10px 16px',
      font: '600 20px/1.3 -apple-system, system-ui, sans-serif', color: '#fff',
      background: 'rgba(20,20,20,.82)', borderRadius: '8px',
    })
    document.body.appendChild(el)
  }, text)
}

// Scroll smoothly so the 1280x720 SVG fills the viewport.
async function toMap(page) {
  await page.evaluate(async () => {
    const svg = document.querySelector('svg')
    const target = svg.getBoundingClientRect().top + window.scrollY - 45
    const from = window.scrollY
    const steps = 45
    for (let i = 1; i <= steps; i++) {
      const t = i / steps
      window.scrollTo(0, from + (target - from) * (1 - Math.pow(1 - t, 3)))
      await new Promise((r) => setTimeout(r, 16))
    }
  })
}

async function showResult(page, selector) {
  await page.locator(selector).scrollIntoViewIfNeeded()
  await page.locator(selector).evaluate((el) => (el.style.background = '#fff3a8'))
  await pause(page, 2000)
}

// Scroll to the form, type each field like a user would, then submit.
async function fill(page, form, fields) {
  const f = page.locator(form)
  await f.scrollIntoViewIfNeeded()
  await pause(page, 600)
  for (const [name, value] of Object.entries(fields)) {
    await f.locator(`input[name="${name}"]`).click()
    await page.keyboard.type(value, { delay: 45 })
  }
  await pause(page, 500)
  await Promise.all([page.waitForLoadState('load'), f.locator('input[type="submit"]').click()])
  await page.waitForLoadState('load')
}

async function waitForServer() {
  for (let i = 0; i < 120; i++) {
    try {
      if ((await fetch(BASE)).ok) return
    } catch {}
    await new Promise((r) => setTimeout(r, 1000))
  }
  throw new Error('server did not start')
}

if (process.argv.length <= 2) rmSync(out, { recursive: true, force: true })
mkdirSync(out, { recursive: true })
const server = spawn('racket', ['src/server.rkt', 'maps/bordeaux.osm'], { cwd: root, stdio: 'inherit' })
try {
  await waitForServer()
  const browser = await chromium.launch()
  const only = process.argv.slice(2)
  for (const scene of scenes.filter((s) => !only.length || only.includes(s.name))) {
    const dir = join(out, scene.name)
    mkdirSync(dir, { recursive: true })
    const context = await browser.newContext({ viewport: SIZE })
    const page = await context.newPage()
    // Every page load gets the caption again.
    page.on('load', () => caption(page, scene.caption).catch(() => {}))
    const frames = []
    let recording = true
    const t0 = Date.now()
    const grab = (async () => {
      while (recording) {
        try {
          frames.push({ t: Date.now() - t0, png: await page.screenshot({ type: 'png' }) })
        } catch {
          await new Promise((r) => setTimeout(r, 20)) // page is navigating
        }
      }
    })()
    await page.goto('about:blank')
    await scene.run(page)
    recording = false
    await grab
    await context.close()
    // ffmpeg concat list: each frame lasts until the next one.
    const kept = frames.filter((f) => f.t > 0)
    const lines = kept.map((f, i) => {
      writeFileSync(join(dir, `${String(i).padStart(5, '0')}.png`), f.png)
      const next = kept[i + 1]?.t ?? f.t + 100
      return `file '${String(i).padStart(5, '0')}.png'\nduration ${((next - f.t) / 1000).toFixed(3)}`
    })
    // The concat demuxer ignores the last duration unless the last file is repeated.
    lines.push(`file '${String(kept.length - 1).padStart(5, '0')}.png'`)
    writeFileSync(join(dir, 'frames.txt'), lines.join('\n') + '\n')
    console.log(`recorded ${scene.name}: ${kept.length} frames, ${(kept.at(-1).t / 1000).toFixed(1)} s`)
  }
  await browser.close()
} finally {
  server.kill()
}
