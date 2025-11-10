---
id: kubernetes-agent-collaboration-setup
title: "Kubernetes Agent Collaboration Infrastructure Setup"
level: operational
type: summary
tags: [kubernetes, minikube, agent-collaboration, infrastructure, deployment]
keywords: [kubernetes, minikube, agent-collaboration, configmap, services, deployments, network-policy, ingress, docker-images]
prerequisites: [docker-setup, kubernetes-basics]
enables: [agent-collaboration-deployment, multi-agent-coordination]
related: [docker-compose-setup, kubernetes-deployment-guide, agent-service-architecture]
readingTime: 15
difficulty: 3
blackboard:
  status: active
  assignedAgent: "4D-Network-Agent"
  lastUpdate: 2025-11-10
  dependencies: [docker-images, kubernetes-cluster]
  watchers: ["5D-Consensus-Agent", "6D-Intelligence-Agent"]
  r5rsEngine: null
  selfBuilding:
    enabled: false
---

# Kubernetes Agent Collaboration Infrastructure Setup

## Summary

Agent collaboration configuration successfully applied to minikube cluster. Complete infrastructure created and configured for multi-agent coordination.

## Infrastructure Created

### 1. ConfigMap
- **agent-collaboration-config**: Configuration for agent coordination and collaboration settings

### 2. Services (4 total)
- **agent-service** (ClusterIP): Main agent service endpoint
- **agent-service-headless**: Headless service for DNS-based service discovery
- **agent-coordination-service**: Service for inter-agent coordination
- **agent-registry-service**: Service for agent registration and discovery

### 3. Deployments (2 total)
- **agent-service-deployment**: 3 replicas for high availability
- **agent-coordination-deployment**: 2 replicas for coordination tasks

### 4. Network Policy
- **agent-collaboration-policy**: Network policy controlling inter-pod communication

### 5. Ingress
- **agent-api-ingress**: Configured and synced for external access to agent API

## Current Status

### Infrastructure
✅ **Ready** - All Kubernetes resources created successfully

### Pods
⚠️ **Waiting for Docker Images** - Pods in `ImagePullBackOff` state

**Issue**: Pods reference `automaton-backend:latest`, which needs to be built and loaded into minikube.

## Docker Image Requirements

The Kubernetes deployments require the following Docker images:
- `automaton-backend:latest` - Backend API service with agent capabilities
- Images must be available in minikube's Docker daemon

## Next Steps

### Option 1: Build and Load Docker Image (Recommended)

```bash
# Build backend image locally
docker build -t automaton-backend:latest -f Dockerfile.backend .

# Load into minikube Docker daemon
minikube image load automaton-backend:latest

# Restart deployments to pick up new image
kubectl rollout restart deployment/agent-service-deployment -n automaton
kubectl rollout restart deployment/agent-coordination-deployment -n automaton
```

### Option 2: Use Existing Backend Image

```bash
# Get existing backend image from another deployment
BACKEND_IMAGE=$(kubectl get deployment backend-deployment -n automaton \
  -o jsonpath='{.spec.template.spec.containers[0].image}' 2>/dev/null || \
  echo "automaton-backend:latest")

# Update agent deployments to use existing image
kubectl set image deployment/agent-service-deployment \
  agent-service=$BACKEND_IMAGE -n automaton

kubectl set image deployment/agent-coordination-deployment \
  agent-coordination=$BACKEND_IMAGE -n automaton
```

### Option 3: Use Minikube's Docker Environment

```bash
# Use minikube's Docker daemon for building
eval $(minikube docker-env)
docker build -t automaton-backend:latest -f Dockerfile.backend .
eval $(minikube docker-env -u)

# Restart deployments
kubectl rollout restart deployment/agent-service-deployment -n automaton
kubectl rollout restart deployment/agent-coordination-deployment -n automaton
```

## Verification

### Check Pod Status

```bash
# Check agent service pods
kubectl get pods -n automaton -l component=agent-service

# Check coordination pods
kubectl get pods -n automaton -l component=agent-coordination

# View pod logs if issues
kubectl logs -n automaton -l component=agent-service --tail=50
```

### Verify Services

```bash
# List all services
kubectl get svc -n automaton

# Test service endpoints
kubectl port-forward -n automaton svc/agent-service 8080:3000
curl http://localhost:8080/api/status
```

## Documentation Created

- **`k8s/agent-collaboration.yaml`**: Complete Kubernetes configuration
- **`k8s/AGENT_COLLABORATION_README.md`**: Full documentation and usage guide
- **`k8s/AGENT_COLLABORATION_STATUS.md`**: Current status and troubleshooting
- **`k8s/MINIKUBE_TROUBLESHOOTING.md`**: Minikube-specific troubleshooting guide

## Docker Build Context

The backend Docker image (`Dockerfile.backend`) includes:
- Multi-stage build (builder + production)
- TypeScript compilation
- All source directories: `src/`, `evolutions/`, `grok_files/`, `ui/`
- All JSONL automaton files
- `AGENTS.md` documentation file
- Health checks and non-root user setup

## Related Docker Configuration

See `docker-compose.yml` for local development setup:
- Backend: Ports 3000-3001
- Frontend: Port 8080
- Grafana: Port 3002
- Prometheus: Port 9090
- Redis: Port 6379

## Status

✅ **Infrastructure Ready** - All Kubernetes resources created  
⏳ **Waiting for Images** - Docker images need to be built and loaded  
📋 **Documentation Complete** - All guides and troubleshooting docs created

Once Docker images are available, the pods will start and agent collaboration will be operational.
