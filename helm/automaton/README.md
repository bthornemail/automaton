# Automaton Helm Chart

A comprehensive Helm chart for deploying the Computational Topology Canvas application on Kubernetes.

## 🚀 Quick Start

### Prerequisites

- Kubernetes 1.20+
- Helm 3.0+
- Ingress controller (nginx recommended)

### Installation

```bash
# Add the repository
helm repo add automaton https://charts.automaton.dev
helm repo update

# Install the chart
helm install my-automaton automaton/automaton \
  --namespace automaton \
  --create-namespace

# Or install from local directory
helm install my-automaton ./helm/automaton \
  --namespace automaton \
  --create-namespace
```

### Upgrade

```bash
helm upgrade my-automaton automaton/automaton \
  --namespace automaton
```

### Uninstall

```bash
helm uninstall my-automaton --namespace automaton
```

## ⚙️ Configuration

### Global Settings

```yaml
global:
  imageRegistry: "your-registry.com"
  storageClass: "fast-ssd"
```

### Image Configuration

```yaml
image:
  registry: docker.io
  repository: automaton
  tag: "v1.0.0"
  pullPolicy: IfNotPresent
```

### Backend Configuration

```yaml
backend:
  replicaCount: 3
  resources:
    limits:
      cpu: 500m
      memory: 512Mi
    requests:
      cpu: 250m
      memory: 256Mi
  env:
    - name: NODE_ENV
      value: "production"
    - name: LOG_LEVEL
      value: "info"
```

### Frontend Configuration

```yaml
frontend:
  replicaCount: 2
  resources:
    limits:
      cpu: 200m
      memory: 256Mi
    requests:
      cpu: 100m
      memory: 128Mi
```

### Ingress Configuration

```yaml
ingress:
  enabled: true
  className: "nginx"
  annotations:
    cert-manager.io/cluster-issuer: "letsencrypt-prod"
    nginx.ingress.kubernetes.io/rate-limit: "100"
  hosts:
    - host: automaton.example.com
      paths:
        - path: /
          pathType: Prefix
  tls:
    - secretName: automaton-tls
      hosts:
        - automaton.example.com
```

### Autoscaling

```yaml
autoscaling:
  enabled: true
  minReplicas: 2
  maxReplicas: 10
  targetCPUUtilizationPercentage: 80
  targetMemoryUtilizationPercentage: 80
```

### Monitoring

```yaml
monitoring:
  enabled: true
  serviceMonitor:
    enabled: true
    interval: 30s
    scrapeTimeout: 10s
```

### Security

```yaml
security:
  networkPolicy:
    enabled: true
  rbac:
    enabled: true
  podSecurityPolicy:
    enabled: true
```

## 🔧 Advanced Configuration

### Custom Values File

Create a `values-production.yaml`:

```yaml
# Production settings
replicaCount: 5

image:
  tag: "v1.2.3"

resources:
  limits:
    cpu: 1000m
    memory: 1Gi
  requests:
    cpu: 500m
    memory: 512Mi

ingress:
  hosts:
    - host: automaton.prod.example.com
      paths:
        - path: /
          pathType: Prefix

autoscaling:
  enabled: true
  minReplicas: 3
  maxReplicas: 20

monitoring:
  enabled: true
```

Deploy with custom values:

```bash
helm install my-automaton ./helm/automaton \
  --namespace automaton \
  --create-namespace \
  --values values-production.yaml
```

### Secrets Management

```yaml
secrets:
  redisPassword: "your-redis-password"
  jwtSecret: "your-jwt-secret"
  apiKeys: |
    {
      "openai": "your-openai-key",
      "anthropic": "your-anthropic-key"
    }
```

### Persistence

```yaml
persistence:
  enabled: true
  storageClass: "fast-ssd"
  size: 100Gi
```

## 📊 Monitoring and Observability

### Prometheus Metrics

The chart automatically configures ServiceMonitor for Prometheus:

```yaml
monitoring:
  serviceMonitor:
    enabled: true
    interval: 30s
    scrapeTimeout: 10s
```

### Grafana Dashboard

Import the provided Grafana dashboard for comprehensive monitoring:

1. Access Grafana via the Ingress
2. Import dashboard from `dashboards/automaton-grafana.json`

## 🔒 Security Features

### Network Policies

Network policies restrict pod communication:

```yaml
security:
  networkPolicy:
    enabled: true
```

### RBAC

Role-based access control:

```yaml
security:
  rbac:
    enabled: true
```

### Pod Security

Security contexts and policies:

```yaml
podSecurityContext:
  runAsNonRoot: true
  runAsUser: 1000
  fsGroup: 1000

securityContext:
  allowPrivilegeEscalation: false
  capabilities:
    drop:
    - ALL
  readOnlyRootFilesystem: true
```

## 🚨 Troubleshooting

### Common Issues

1. **Pods not starting**: Check resource limits and requests
2. **Ingress not working**: Verify Ingress controller and DNS
3. **Redis connection**: Check Redis password and network policies

### Debug Commands

```bash
# Check pod status
kubectl get pods -n automaton

# Check pod logs
kubectl logs -f deployment/my-automaton-backend -n automaton

# Check events
kubectl get events -n automaton --sort-by='.lastTimestamp'

# Describe pod
kubectl describe pod <pod-name> -n automaton

# Port forward for debugging
kubectl port-forward svc/my-automaton-backend 5555:5555 -n automaton
```

### Helm Debug

```bash
# Dry run installation
helm install my-automaton ./helm/automaton \
  --namespace automaton \
  --dry-run --debug

# Get rendered manifests
helm get manifest my-automaton -n automaton

# Check values
helm get values my-automaton -n automaton
```

## 🔄 CI/CD Integration

### GitHub Actions

```yaml
name: Deploy to Kubernetes

on:
  push:
    branches: [main]

jobs:
  deploy:
    runs-on: ubuntu-latest
    steps:
    - uses: actions/checkout@v3
    
    - name: Configure kubectl
      uses: azure/k8s-set-context@v1
      with:
        method: kubeconfig
        kubeconfig: ${{ secrets.KUBE_CONFIG }}
    
    - name: Deploy with Helm
      run: |
        helm upgrade --install my-automaton ./helm/automaton \
          --namespace automaton \
          --create-namespace \
          --set image.tag=${{ github.sha }}
```

## 📚 Additional Resources

- [Helm Documentation](https://helm.sh/docs/)
- [Kubernetes Documentation](https://kubernetes.io/docs/)
- [Ingress-Nginx Controller](https://kubernetes.github.io/ingress-nginx/)
- [Prometheus Operator](https://github.com/prometheus-operator/prometheus-operator)

## 🤝 Contributing

1. Fork the repository
2. Create a feature branch
3. Make your changes
4. Test with `helm template`
5. Submit a pull request

## 📄 License

This project is licensed under the MIT License - see the [LICENSE](LICENSE) file for details.