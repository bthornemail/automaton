/**
 * Slides API Routes
 * 
 * Backend API endpoints for saving edited slides to evolution directories.
 */

import { Router, Request, Response } from 'express';
import * as fs from 'fs/promises';
import * as path from 'path';
import { errorLoggingService } from '../../services/error-logging-service';

const router = Router();

/**
 * POST /api/slides/save
 * Save a slide file to the evolution directory.
 */
router.post('/save', async (req: Request, res: Response) => {
  try {
    const { filePath, content } = req.body;

    if (!filePath || !content) {
      return res.status(400).json({
        error: 'Missing required fields: filePath and content'
      });
    }

    // Validate file path (prevent directory traversal)
    const normalizedPath = path.normalize(filePath);
    if (normalizedPath.includes('..')) {
      return res.status(400).json({
        error: 'Invalid file path'
      });
    }

    // Ensure directory exists
    const dirPath = path.dirname(normalizedPath);
    await fs.mkdir(dirPath, { recursive: true });

    // Write file
    await fs.writeFile(normalizedPath, content, 'utf-8');

    res.json({
      success: true,
      filePath: normalizedPath
    });
  } catch (error) {
    const errorObj = error instanceof Error ? error : new Error(String(error));
    errorLoggingService.logError(errorObj, {
      service: 'SlidesAPI',
      action: 'save',
      metadata: { filePath: req.body?.filePath },
      severity: 'error'
    });

    res.status(500).json({
      error: errorObj.message
    });
  }
});

/**
 * GET /api/slides/:evolutionPath
 * Get saved slides from an evolution directory.
 */
router.get('/:evolutionPath(*)', async (req: Request, res: Response) => {
  try {
    const evolutionPath = req.params.evolutionPath;
    const slidesDir = path.join(evolutionPath, 'slides');
    const indexPath = path.join(slidesDir, 'slides-index.json');

    // Check if slides directory exists
    try {
      await fs.access(slidesDir);
    } catch {
      return res.json({
        slides: [],
        message: 'No saved slides found'
      });
    }

    // Read slides index
    let index: any = { slides: [] };
    try {
      const indexContent = await fs.readFile(indexPath, 'utf-8');
      index = JSON.parse(indexContent);
    } catch {
      // Index doesn't exist, return empty
      return res.json({
        slides: [],
        message: 'No slides index found'
      });
    }

    // Read all slide files
    const slides = [];
    for (const slideId of index.slides) {
      try {
        const slidePath = path.join(slidesDir, `slide-${slideId}.json`);
        const slideContent = await fs.readFile(slidePath, 'utf-8');
        slides.push(JSON.parse(slideContent));
      } catch (error) {
        console.warn(`Failed to load slide ${slideId}:`, error);
      }
    }

    res.json({
      slides,
      index
    });
  } catch (error) {
    const errorObj = error instanceof Error ? error : new Error(String(error));
    errorLoggingService.logError(errorObj, {
      service: 'SlidesAPI',
      action: 'get',
      metadata: { evolutionPath: req.params.evolutionPath },
      severity: 'error'
    });

    res.status(500).json({
      error: errorObj.message
    });
  }
});

export default router;

