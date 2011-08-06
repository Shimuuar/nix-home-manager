
----------------------------------------------------------------
require 'cairo'

----------------------------------------------------------------
-- Globals
cs = nil                        -- Cairo surface
bg_colour = 0xEEEEEE            -- Background color
bg_alpha  = 0.75                 -- Desired alpha
corner_r  = 20                  -- Corener radius
gap       = 4                   -- Gap around text block

function conky_cairo_cleanup()
   if( cs ~= nil ) then
      cairo_surface_destroy( cs )
      cs = nil
   end
end


function rgb_to_r_g_b(colour,alpha)
   return
     ((colour / 0x10000) % 0x100) / 255. ,
     ((colour / 0x100  ) % 0x100) / 255. ,
     (colour             % 0x100) / 255. ,
     alpha
end


function conky_draw_round_rect( cr, x0, y0, w, h, corner ) 
   if (w == 0) or (h == 0) then return end
   local x1 = x0 + w
   local y1 = y0 + h
   local r  = math.min( corner, w, h )

   cairo_move_to  (cr, x0, y0 + r)
   cairo_curve_to (cr, x0 , y0, x0 , y0, x0 + r, y0)
   cairo_line_to (cr, x1 - r, y0)
   cairo_curve_to (cr, x1, y0, x1, y0, x1, y0 + r)
   cairo_line_to (cr, x1 , y1 - r)
   cairo_curve_to (cr, x1, y1, x1, y1, x1 - r, y1)
   cairo_line_to (cr, x0 + r, y1)
   cairo_curve_to (cr, x0, y1, x0, y1, x0, y1- r)

   cairo_close_path (cr)

   cairo_set_source_rgba(cr,rgb_to_r_g_b(bg_colour,bg_alpha))
   cairo_fill(cr)
end
 

function conky_draw_bg()
    if conky_window == nil then return end
    if cs == nil
       or cairo_xlib_surface_get_width(cs) ~= conky_window.width
       or cairo_xlib_surface_get_height(cs) ~= conky_window.height 
    then
       conky_cairo_cleanup()
       cs = cairo_xlib_surface_create(conky_window.display, conky_window.drawable, conky_window.visual, conky_window.width, conky_window.height)
    end

    local cr = cairo_create(cs)
    conky_draw_round_rect( 
       cr, 
       conky_window.text_start_x - gap,
       conky_window.text_start_y - gap,
       conky_window.text_width   + gap*2,
       conky_window.text_height  + gap*2,
       corner_r )
    cairo_destroy(cr)    
end