---
id: voice-demo
type: canvasl-template
dimension: 2

adjacency:
  edges:
    - speech-input
    - macro-execution
    - web-api-call
    - speech-output
  orientation: [1, 1, 1, -1]

speech:
  input:
    lang: en-US
    continuous: true
    interimResults: true
    keywords:
      - location
      - notify
      - save
      - copy
      - battery
  output:
    voice: Google US English
    rate: 1.0
    pitch: 1.0

macros:
  - keyword: location
    api: geolocation
    method: getCurrentPosition
    params:
      enableHighAccuracy: true
    type: [web_api, geolocation]

  - keyword: notify
    api: notifications
    method: showNotification
    params:
      title: CANVASL Alert
      body: Voice command executed
    type: [web_api, notifications]

  - keyword: save
    api: indexeddb
    method: put
    params:
      store: voice-data
      key: last-command
    type: [web_api, indexeddb]

  - keyword: copy
    api: clipboard
    method: writeText
    params:
      text: Hello from CANVASL Voice App
    type: [web_api, clipboard]

  - keyword: battery
    api: battery
    method: getBattery
    params: {}
    type: [web_api, battery]

validates:
  homology: true
  byzantine: false
  accessibility: true

features:
  version: 1.0.0
  category: voice-controlled-app
  description: Demo voice application with Web API integration
---

# Voice-Controlled CANVASL Demo

This template creates a voice-controlled web application with access to:
- **Geolocation** - Say "location" to get GPS coordinates
- **Notifications** - Say "notify" to send a browser notification
- **Storage** - Say "save" to store session data
- **Clipboard** - Say "copy" to copy text to clipboard
- **Battery** - Say "battery" to check battery status

## Usage

1. Start the application
2. Grant microphone and notification permissions
3. Say any of the keywords to trigger actions
4. The app will speak the results back to you

## Homological Structure

This template is a 2-cell (face) in the chain complex with:
- **C₀ (vertices)**: Keywords {location, notify, save, copy, battery}
- **C₁ (edges)**: Connections from keywords to Web APIs
- **C₂ (faces)**: This document template
- **C₃ (solids)**: Interface triples for each macro
- **C₄ (hypervolumes)**: Execution history and evolution contexts

The boundary operator ∂₂ maps this template to its edge cycle, ensuring topological consistency.
