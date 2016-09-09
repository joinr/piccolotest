(ns piccolotest.animation)


(defn animate-to-bounds [nd x y width height duration]
  (if (zero? duration)
    (do (.setBounds nd x y width height)
        nd)
    (let [dst (PBounds. x y width height)
          interpolatingActivity (PInterpolatingActivity. duration PUtil/DEFAULT_ACTIVITY_STEP_RATE)
    
    public PInterpolatingActivity animateToBounds(final double x, final double y, final double width,
            final double height, final long duration) {
        final PBounds dst = new PBounds(x, y, width, height);

        final PInterpolatingActivity interpolatingActivity = new PInterpolatingActivity(duration,
                PUtil.DEFAULT_ACTIVITY_STEP_RATE) {
            private PBounds src;

            protected void activityStarted() {
                src = getBounds();
                startResizeBounds();
                super.activityStarted();
            }

            public void setRelativeTargetValue(final float zeroToOne) {
                PNode.this.setBounds(src.x + zeroToOne * (dst.x - src.x), src.y + zeroToOne * (dst.y - src.y),
                        src.width + zeroToOne * (dst.width - src.width), src.height + zeroToOne
                                * (dst.height - src.height));
            }

            protected void activityFinished() {
                super.activityFinished();
                endResizeBounds();
            }
        };

        addActivity(interpolatingActivity);
        return interpolatingActivity;
    }
