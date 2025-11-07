{{- /*
Expand the name of the chart.
*/}}
{{- define "automaton.name" -}}
{{- default .Chart.Name .Values.nameOverride | trunc 63 | trimSuffix "-" }}
{{- end }}

{{- /*
Create a default fully qualified app name.
*/}}
{{- define "automaton.fullname" -}}
{{- if .Values.fullnameOverride }}
{{- .Values.fullnameOverride | trunc 63 | trimSuffix "-" }}
{{- else }}
{{- $name := default .Chart.Name .Values.nameOverride }}
{{- if contains $name .Release.Name }}
{{- .Release.Name | trunc 63 | trimSuffix "-" }}
{{- else }}
{{- printf "%s-%s" .Release.Name $name | trunc 63 | trimSuffix "-" }}
{{- end }}
{{- end }}
{{- end }}

{{- /*
Create chart name and version as used by the chart label.
*/}}
{{- define "automaton.chart" -}}
{{- printf "%s-%s" .Chart.Name .Chart.Version | replace "+" "_" | trunc 63 | trimSuffix "-" }}
{{- end }}

{{- /*
Common labels
*/}}
{{- define "automaton.labels" -}}
helm.sh/chart: {{ include "automaton.chart" . }}
{{ include "automaton.selectorLabels" . }}
{{- if .Chart.AppVersion }}
app.kubernetes.io/version: {{ .Chart.AppVersion | quote }}
{{- end }}
app.kubernetes.io/managed-by: {{ .Release.Service }}
{{- end }}

{{- /*
Selector labels
*/}}
{{- define "automaton.selectorLabels" -}}
app.kubernetes.io/name: {{ include "automaton.name" . }}
app.kubernetes.io/instance: {{ .Release.Name }}
{{- end }}

{{- /*
Create the name of the service account to use
*/}}
{{- define "automaton.serviceAccountName" -}}
{{- if .Values.serviceAccount.create }}
{{- default (include "automaton.fullname" .) .Values.serviceAccount.name }}
{{- else }}
{{- default "default" .Values.serviceAccount.name }}
{{- end }}
{{- end }}

{{- /*
Backend image
*/}}
{{- define "automaton.backend.image" -}}
{{- $registry := .Values.global.imageRegistry | default .Values.image.registry }}
{{- $repository := .Values.backend.image.repository | default "automaton/backend" }}
{{- $tag := .Values.backend.image.tag | default .Values.image.tag | default "latest" }}
{{- printf "%s/%s:%s" $registry $repository $tag }}
{{- end }}

{{- /*
Frontend image
*/}}
{{- define "automaton.frontend.image" -}}
{{- $registry := .Values.global.imageRegistry | default .Values.image.registry }}
{{- $repository := .Values.frontend.image.repository | default "automaton/frontend" }}
{{- $tag := .Values.frontend.image.tag | default .Values.image.tag | default "latest" }}
{{- printf "%s/%s:%s" $registry $repository $tag }}
{{- end }}