/**
 * Virtualized Card List Component
 * 
 * Efficiently renders large lists of cards using virtual scrolling
 */

import React, { useState, useEffect, useRef, useMemo } from 'react';
import { Card } from './Card';
import { motion } from 'framer-motion';

export interface VirtualizedCardListProps {
  cards: any[];
  itemHeight?: number;
  overscan?: number;
  onCardSelect?: (card: any) => void;
  className?: string;
}

export const VirtualizedCardList: React.FC<VirtualizedCardListProps> = ({
  cards,
  itemHeight = 200,
  overscan = 3,
  onCardSelect,
  className = ''
}) => {
  const [scrollTop, setScrollTop] = useState(0);
  const [containerHeight, setContainerHeight] = useState(0);
  const containerRef = useRef<HTMLDivElement>(null);

  // Calculate visible range
  const visibleRange = useMemo(() => {
    const startIndex = Math.max(0, Math.floor(scrollTop / itemHeight) - overscan);
    const endIndex = Math.min(
      cards.length - 1,
      Math.ceil((scrollTop + containerHeight) / itemHeight) + overscan
    );
    return { startIndex, endIndex };
  }, [scrollTop, containerHeight, itemHeight, overscan, cards.length]);

  // Calculate total height
  const totalHeight = cards.length * itemHeight;

  // Calculate offset for visible items
  const offsetY = visibleRange.startIndex * itemHeight;

  // Visible cards
  const visibleCards = useMemo(() => {
    return cards.slice(visibleRange.startIndex, visibleRange.endIndex + 1);
  }, [cards, visibleRange.startIndex, visibleRange.endIndex]);

  // Update container height on resize
  useEffect(() => {
    const updateHeight = () => {
      if (containerRef.current) {
        setContainerHeight(containerRef.current.clientHeight);
      }
    };

    updateHeight();
    window.addEventListener('resize', updateHeight);
    return () => window.removeEventListener('resize', updateHeight);
  }, []);

  // Handle scroll
  const handleScroll = (e: React.UIEvent<HTMLDivElement>) => {
    setScrollTop(e.currentTarget.scrollTop);
  };

  // Keyboard navigation
  useEffect(() => {
    const handleKeyDown = (e: KeyboardEvent) => {
      if (!containerRef.current) return;

      const { startIndex, endIndex } = visibleRange;
      let newIndex = startIndex;

      switch (e.key) {
        case 'ArrowDown':
          e.preventDefault();
          newIndex = Math.min(cards.length - 1, startIndex + 1);
          break;
        case 'ArrowUp':
          e.preventDefault();
          newIndex = Math.max(0, startIndex - 1);
          break;
        case 'PageDown':
          e.preventDefault();
          newIndex = Math.min(cards.length - 1, startIndex + Math.floor(containerHeight / itemHeight));
          break;
        case 'PageUp':
          e.preventDefault();
          newIndex = Math.max(0, startIndex - Math.floor(containerHeight / itemHeight));
          break;
        default:
          return;
      }

      if (newIndex !== startIndex) {
        const newScrollTop = newIndex * itemHeight;
        containerRef.current.scrollTop = newScrollTop;
        setScrollTop(newScrollTop);
      }
    };

    window.addEventListener('keydown', handleKeyDown);
    return () => window.removeEventListener('keydown', handleKeyDown);
  }, [visibleRange, cards.length, containerHeight, itemHeight]);

  return (
    <div
      ref={containerRef}
      className={`overflow-auto ${className}`}
      onScroll={handleScroll}
      style={{ height: '100%' }}
      tabIndex={0}
    >
      <div style={{ height: totalHeight, position: 'relative' }}>
        <div
          style={{
            transform: `translateY(${offsetY}px)`,
            position: 'absolute',
            top: 0,
            left: 0,
            right: 0
          }}
        >
          {visibleCards.map((card, index) => {
            const actualIndex = visibleRange.startIndex + index;
            return (
              <motion.div
                key={card.id || actualIndex}
                initial={{ opacity: 0, y: 20 }}
                animate={{ opacity: 1, y: 0 }}
                transition={{ duration: 0.2 }}
                style={{ height: itemHeight }}
                onClick={() => onCardSelect?.(card)}
              >
                <Card
                  {...card}
                  className="h-full cursor-pointer hover:bg-gray-800 transition-colors"
                />
              </motion.div>
            );
          })}
        </div>
      </div>
    </div>
  );
};

