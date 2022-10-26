#include <stdio.h>
#include "agg_basics.h"
#include "agg_rendering_buffer.h"
#include "agg_rasterizer_scanline_aa.h"
#include "agg_scanline_u.h"
#include "agg_scanline_p.h"
#include "agg_renderer_scanline.h"
#include "agg_renderer_primitives.h"
#include "agg_conv_curve.h"
#include "agg_conv_stroke.h"
#include "agg_conv_clip_polygon.h"
#include "agg_gsv_text.h"
#include "agg_pixfmt_rgb.h"
#include "agg_platform_support.h"

#include "agg_slider_ctrl.h"
#include "agg_cbox_ctrl.h"
#include "agg_rbox_ctrl.h"

#include "agg_conv_clipper.h"
#include "windows.h"

enum flip_y_e { flip_y = true };


class spiral
{
public:
    spiral(double x, double y, double r1, double r2, double step, double start_angle=0) :
        m_x(x), 
        m_y(y), 
        m_r1(r1), 
        m_r2(r2), 
        m_step(step), 
        m_start_angle(start_angle),
        m_angle(start_angle),
        m_da(agg::deg2rad(4.0)),
        m_dr(m_step / 90.0)
    {
    }

    void rewind(unsigned) 
    { 
        m_angle = m_start_angle; 
        m_curr_r = m_r1; 
        m_start = true; 
    }

    unsigned vertex(double* x, double* y)
    {
        if(m_curr_r > m_r2) return agg::path_cmd_stop;

        *x = m_x + cos(m_angle) * m_curr_r;
        *y = m_y + sin(m_angle) * m_curr_r;
        m_curr_r += m_dr;
        m_angle += m_da;
        if(m_start) 
        {
            m_start = false;
            return agg::path_cmd_move_to;
        }
        return agg::path_cmd_line_to;
    }

private:
    double m_x;
    double m_y;
    double m_r1;
    double m_r2;
    double m_step;
    double m_start_angle;

    double m_angle;
    double m_curr_r;
    double m_da;
    double m_dr;
    bool   m_start;
};



namespace agg
{
    // A simple counter of points and contours
    template<class Src> struct conv_poly_counter
    {
        unsigned m_contours;
        unsigned m_points;
    
        conv_poly_counter(Src& src) : m_src(&src), m_contours(0), m_points(0) {}
    
        void rewind(unsigned path_id)
        {
            m_contours = 0;
            m_points = 0;
            m_src->rewind(path_id);
        }

        unsigned vertex(double* x, double* y)
        {
            unsigned cmd = m_src->vertex(x, y);
            if(is_vertex(cmd))  ++m_points;
            if(is_move_to(cmd)) ++m_contours;
            return cmd;
        }

    private:
        Src* m_src;
    };
}


void make_gb_poly(agg::path_storage& ps);
void make_arrows(agg::path_storage& ps);


class the_application : public agg::platform_support
{
    agg::rbox_ctrl<agg::rgba8> m_polygons;
    agg::rbox_ctrl<agg::rgba8> m_operation;
    double m_x;
    double m_y;

	virtual void on_key(int x, int y, unsigned key, unsigned flags)
	{
		if(key == agg::key_escape) exit(0);
		
	}

public:
    the_application(agg::pix_format_e format, bool flip_y) :
        agg::platform_support(format, flip_y),
        m_polygons (5.0,     5.0, 5.0+205.0,  110.0,  !flip_y),
        m_operation(555.0,   5.0, 555.0+80.0, 130.0,  !flip_y)
    {
        m_operation.add_item("None");
        m_operation.add_item("OR");
        m_operation.add_item("AND");
        m_operation.add_item("XOR");
        m_operation.add_item("A-B");
        m_operation.add_item("B-A");
        m_operation.cur_item(2);
        add_ctrl(m_operation);

        m_polygons.add_item("Two Simple Paths");
        m_polygons.add_item("Closed Stroke");
        m_polygons.add_item("Great Britain and Arrows");
        m_polygons.add_item("Great Britain and Spiral");
        m_polygons.add_item("Spiral and Glyph");
        m_polygons.cur_item(3);
        add_ctrl(m_polygons);
    }


    template<class Scanline, class Ras, class Ren, class Clp>
    void perform_rendering(Scanline &sl, Ras &ras, Ren &ren, Clp &clp)
    {
        if(m_operation.cur_item() > 0)
        {
            ras.reset();
            switch(m_operation.cur_item())
            {
                case 1: clp.operation(agg::clipper_or);           break;
                case 2: clp.operation(agg::clipper_and);          break;
                case 3: clp.operation(agg::clipper_xor);          break;
                case 4: clp.operation(agg::clipper_a_minus_b);    break;
                case 5: clp.operation(agg::clipper_b_minus_a);    break;
            }
            agg::conv_poly_counter<Clp> counter(clp);


            start_timer();
            counter.rewind(0);
            double t1 = elapsed_time();

			agg::path_storage ps;
            double x;
            double y;
            unsigned cmd;
            start_timer();
            while(!agg::is_stop(cmd = counter.vertex(&x, &y)))
            {
				if(agg::is_move_to(cmd)) 
					ps.move_to(x, y);
				else if(agg::is_line_to(cmd))
					ps.line_to(x, y);
				else if(agg::is_close(cmd))
					ps.close_polygon();
			}
			ras.add_path(ps);
            ren.color(agg::rgba(0.25, 0.9, 0.25, 0.65));
            agg::render_scanlines(ras, sl, ren);
            double t2 = elapsed_time();

            agg::conv_stroke<agg::path_storage> stroke(ps);
            stroke.width(0.4);
            ras.add_path(stroke);
            ren.color(agg::rgba(0, 0, 0));
            agg::render_scanlines(ras, sl, ren);

            char buf[100];
            sprintf_s(buf, "Contours: %d      Points: %d", counter.m_contours, counter.m_points);
            agg::gsv_text txt;
            agg::conv_stroke<agg::gsv_text> txt_stroke(txt);
            txt_stroke.width(1.5);
            txt_stroke.line_cap(agg::round_cap);
            txt.size(10.0);
            txt.start_point(250, 5);
            txt.text(buf);
            ras.add_path(txt_stroke);
            ren.color(agg::rgba(0.0, 0.0, 0.0));
            agg::render_scanlines(ras, sl, ren);

            sprintf_s(buf, "Clipper=%.3fms Render=%.3fms", t1, t2);
            txt.start_point(250, 20);
            txt.text(buf);
            ras.add_path(txt_stroke);
            ren.color(agg::rgba(0.0, 0.0, 0.0));
            agg::render_scanlines(ras, sl, ren);
        }
    }


    template<class Scanline, class Ras>
    unsigned render_clipper(Scanline& sl, Ras& ras)
    {
        agg::pixfmt_bgr24 pf(rbuf_window());
        agg::renderer_base<agg::pixfmt_bgr24> rb(pf);
        agg::renderer_scanline_aa_solid<agg::renderer_base<agg::pixfmt_bgr24> > ren(rb);


        switch(m_polygons.cur_item())
        {
            case 0:
            {
                //------------------------------------
                // Two simple paths
                //
                agg::path_storage ps1;
                agg::path_storage ps2;

                agg::conv_clipper<agg::path_storage, agg::path_storage> clp(ps1, ps2, agg::clipper_or, agg::clipper_non_zero, agg::clipper_non_zero);

                double x = m_x - initial_width()/2 + 100;
                double y = m_y - initial_height()/2 + 100;
                ps1.move_to(x+140, y+145);
                ps1.line_to(x+225, y+44);
                ps1.line_to(x+296, y+219);
                ps1.close_polygon();

                ps1.line_to(x+226, y+289);
                ps1.line_to(x+82,  y+292);

                ps1.move_to(x+220, y+222);
                ps1.line_to(x+363, y+249);
                ps1.line_to(x+265, y+331);

                ps1.move_to(x+242, y+243);
                ps1.line_to(x+268, y+309);
                ps1.line_to(x+325, y+261);

                ps1.move_to(x+259, y+259);
                ps1.line_to(x+273, y+288);
                ps1.line_to(x+298, y+266);

                ps2.move_to(100+32,  100+77);
                ps2.line_to(100+473, 100+263);
                ps2.line_to(100+351, 100+290);
                ps2.line_to(100+354, 100+374);

                ras.reset();
                ras.add_path(ps1);
                ren.color(agg::rgba(0, 0, 0, 0.1));
                agg::render_scanlines(ras, sl, ren);

                ras.reset();
                ras.add_path(ps2);
                ren.color(agg::rgba(0, 0.6, 0, 0.1));
                agg::render_scanlines(ras, sl, ren);

                perform_rendering(sl, ras, ren, clp);
            }
            break;

            case 1:
            {
                //------------------------------------
                // Closed stroke
                //
                agg::path_storage ps1;
                agg::path_storage ps2;
                agg::conv_stroke<agg::path_storage> stroke(ps2);
                stroke.width(10.0);

                agg::conv_clipper<agg::path_storage, 
                              agg::conv_stroke<agg::path_storage> > clp(ps1, stroke, agg::clipper_or, agg::clipper_non_zero, agg::clipper_non_zero);


                double x = m_x - initial_width()/2 + 100;
                double y = m_y - initial_height()/2 + 100;
                ps1.move_to(x+140, y+145);
                ps1.line_to(x+225, y+44);
                ps1.line_to(x+296, y+219);
                ps1.close_polygon();

                ps1.line_to(x+226, y+289);
                ps1.line_to(x+82,  y+292);

                ps1.move_to(x+220-50, y+222);
                ps1.line_to(x+265-50, y+331);
                ps1.line_to(x+363-50, y+249);
                ps1.close_polygon(agg::path_flags_ccw);

                ps2.move_to(100+32,  100+77);
                ps2.line_to(100+473, 100+263);
                ps2.line_to(100+351, 100+290);
                ps2.line_to(100+354, 100+374);
                ps2.close_polygon();

                ras.reset();
                ras.add_path(ps1);
                ren.color(agg::rgba(0, 0, 0, 0.1));
                agg::render_scanlines(ras, sl, ren);

                ras.reset();
                ras.add_path(stroke);
                ren.color(agg::rgba(0, 0.6, 0, 0.1));
                agg::render_scanlines(ras, sl, ren);

                perform_rendering(sl, ras, ren, clp);
            }
            break;


            case 2:
            {
                //------------------------------------
                // Great Britain and Arrows
                //
                agg::path_storage gb_poly;
                agg::path_storage arrows;
                make_gb_poly(gb_poly);
                make_arrows(arrows);

                agg::trans_affine mtx1;
                agg::trans_affine mtx2;
                mtx1 *= agg::trans_affine_translation(-1150, -1150);
                mtx1 *= agg::trans_affine_scaling(2.0);

                mtx2 = mtx1;
                mtx2 *= agg::trans_affine_translation(m_x - initial_width()/2, 
                                                      m_y - initial_height()/2);

                agg::conv_transform<agg::path_storage> trans_gb_poly(gb_poly, mtx1);
                agg::conv_transform<agg::path_storage> trans_arrows(arrows, mtx2);

                agg::conv_clipper<agg::conv_transform<agg::path_storage>, 
                              agg::conv_transform<agg::path_storage> > clp(trans_gb_poly, trans_arrows, agg::clipper_or, agg::clipper_non_zero, agg::clipper_non_zero);

                ras.add_path(trans_gb_poly);
                ren.color(agg::rgba(0.5, 0.5, 0, 0.1));
                agg::render_scanlines(ras, sl, ren);

                agg::conv_stroke<agg::conv_transform<agg::path_storage> > stroke_gb_poly(trans_gb_poly);
                stroke_gb_poly.width(0.1);
                ras.add_path(stroke_gb_poly);
                ren.color(agg::rgba(0, 0, 0));
                agg::render_scanlines(ras, sl, ren);
        
                ras.add_path(trans_arrows);
                ren.color(agg::rgba(0.0, 0.5, 0.5, 0.1));
                agg::render_scanlines(ras, sl, ren);

                perform_rendering(sl, ras, ren, clp);
            }
            break;


            case 3:
            {
                //------------------------------------
                // Great Britain and a Spiral
                //
                spiral sp(m_x, m_y, 10, 150, 30, 0.0);
                agg::conv_stroke<spiral> stroke(sp);
                stroke.width(15.0);

                agg::path_storage gb_poly;
                make_gb_poly(gb_poly);

                agg::trans_affine mtx;
                mtx *= agg::trans_affine_translation(-1150, -1150);
                mtx *= agg::trans_affine_scaling(2.0);

                agg::conv_transform<agg::path_storage> trans_gb_poly(gb_poly, mtx);

                agg::conv_clipper<agg::conv_transform<agg::path_storage>, 
                              agg::conv_stroke<spiral> > clp(trans_gb_poly, stroke, agg::clipper_or, agg::clipper_non_zero, agg::clipper_non_zero);

                ras.add_path(trans_gb_poly);
                ren.color(agg::rgba(0.5, 0.5, 0, 0.1));
                agg::render_scanlines(ras, sl, ren);

                agg::conv_stroke<agg::conv_transform<agg::path_storage> > stroke_gb_poly(trans_gb_poly);
                stroke_gb_poly.width(0.1);
                ras.add_path(stroke_gb_poly);
                ren.color(agg::rgba(0, 0, 0));
                agg::render_scanlines(ras, sl, ren);
        
                ras.add_path(stroke);
                ren.color(agg::rgba(0.0, 0.5, 0.5, 0.1));
                agg::render_scanlines(ras, sl, ren);

                perform_rendering(sl, ras, ren, clp);
            }
            break;


            case 4:
            {
                //------------------------------------
                // Spiral and glyph
                //
                spiral sp(m_x, m_y, 10, 150, 30, 0.0);
                agg::conv_stroke<spiral> stroke(sp);
                stroke.width(15.0);

                agg::path_storage glyph;
                glyph.move_to(28.47, 6.45);
                glyph.curve3(21.58, 1.12, 19.82, 0.29);
                glyph.curve3(17.19, -0.93, 14.21, -0.93);
                glyph.curve3(9.57, -0.93, 6.57, 2.25);
                glyph.curve3(3.56, 5.42, 3.56, 10.60);
                glyph.curve3(3.56, 13.87, 5.03, 16.26);
                glyph.curve3(7.03, 19.58, 11.99, 22.51);
                glyph.curve3(16.94, 25.44, 28.47, 29.64);
                glyph.line_to(28.47, 31.40);
                glyph.curve3(28.47, 38.09, 26.34, 40.58);
                glyph.curve3(24.22, 43.07, 20.17, 43.07);
                glyph.curve3(17.09, 43.07, 15.28, 41.41);
                glyph.curve3(13.43, 39.75, 13.43, 37.60);
                glyph.line_to(13.53, 34.77);
                glyph.curve3(13.53, 32.52, 12.38, 31.30);
                glyph.curve3(11.23, 30.08, 9.38, 30.08);
                glyph.curve3(7.57, 30.08, 6.42, 31.35);
                glyph.curve3(5.27, 32.62, 5.27, 34.81);
                glyph.curve3(5.27, 39.01, 9.57, 42.53);
                glyph.curve3(13.87, 46.04, 21.63, 46.04);
                glyph.curve3(27.59, 46.04, 31.40, 44.04);
                glyph.curve3(34.28, 42.53, 35.64, 39.31);
                glyph.curve3(36.52, 37.21, 36.52, 30.71);
                glyph.line_to(36.52, 15.53);
                glyph.curve3(36.52, 9.13, 36.77, 7.69);
                glyph.curve3(37.01, 6.25, 37.57, 5.76);
                glyph.curve3(38.13, 5.27, 38.87, 5.27);
                glyph.curve3(39.65, 5.27, 40.23, 5.62);
                glyph.curve3(41.26, 6.25, 44.19, 9.18);
                glyph.line_to(44.19, 6.45);
                glyph.curve3(38.72, -0.88, 33.74, -0.88);
                glyph.curve3(31.35, -0.88, 29.93, 0.78);
                glyph.curve3(28.52, 2.44, 28.47, 6.45);
                glyph.close_polygon();

                glyph.move_to(28.47, 9.62);
                glyph.line_to(28.47, 26.66);
                glyph.curve3(21.09, 23.73, 18.95, 22.51);
                glyph.curve3(15.09, 20.36, 13.43, 18.02);
                glyph.curve3(11.77, 15.67, 11.77, 12.89);
                glyph.curve3(11.77, 9.38, 13.87, 7.06);
                glyph.curve3(15.97, 4.74, 18.70, 4.74);
                glyph.curve3(22.41, 4.74, 28.47, 9.62);
                glyph.close_polygon();

                agg::trans_affine mtx;
                mtx *= agg::trans_affine_scaling(4.0);
                mtx *= agg::trans_affine_translation(220, 200);
                agg::conv_transform<agg::path_storage> trans(glyph, mtx);
                agg::conv_curve<agg::conv_transform<agg::path_storage> > curve(trans);

                agg::conv_clipper<agg::conv_stroke<spiral>, 
                                 agg::conv_curve<
                                     agg::conv_transform<
                                         agg::path_storage> > > clp(stroke, curve, agg::clipper_or, agg::clipper_non_zero, agg::clipper_non_zero);

                ras.reset();
                ras.add_path(stroke);
                ren.color(agg::rgba(0, 0, 0, 0.1));
                agg::render_scanlines(ras, sl, ren);

                ras.reset();
                ras.add_path(curve);
                ren.color(agg::rgba(0, 0.6, 0, 0.1));
                agg::render_scanlines(ras, sl, ren);

                perform_rendering(sl, ras, ren, clp);
            }
            break;
        }

        return 0;
    }


    virtual void on_init()
    {
        m_x = width() / 2.0;
        m_y = height() / 2.0;
    }

    virtual void on_draw()
    {
        typedef agg::renderer_base<agg::pixfmt_bgr24> base_ren_type;

        agg::pixfmt_bgr24 pf(rbuf_window());
        base_ren_type ren_base(pf);
        ren_base.clear(agg::rgba(1,1,1));

        agg::scanline_u8 sl;
        agg::rasterizer_scanline_aa<> ras;

        render_clipper(sl, ras);

        agg::render_ctrl(ras, sl, ren_base, m_polygons);
        agg::render_ctrl(ras, sl, ren_base, m_operation);
    }

    virtual void on_mouse_button_down(int x, int y, unsigned flags)
    {
        if(flags & agg::mouse_left)
        {
            m_x = x;
            m_y = y;
            force_redraw();
        }
    }


    virtual void on_mouse_move(int x, int y, unsigned flags)
    {
        if(flags & agg::mouse_left)
        {
            m_x = x;
            m_y = y;
            force_redraw();
        }
    }



};


int agg_main(int argc, char* argv[])
{
    the_application app(agg::pix_format_bgr24, flip_y);
    app.caption("AGG Example. Clipper");

    if(app.init(640, 520, agg::window_resize))
    {
		//replace the main window icon with Resource Icon #1 ...
		HWND w = GetActiveWindow();
		HMODULE m = GetModuleHandle(0); //hInstance
		HANDLE small_ico = LoadImage(m, MAKEINTRESOURCE(1), IMAGE_ICON, 16, 16, 0);
		HANDLE big_ico = LoadImage(m, MAKEINTRESOURCE(1), IMAGE_ICON, 32, 32, 0);
		SendMessage(w, WM_SETICON, ICON_SMALL, (LPARAM)small_ico);
		SendMessage(w, WM_SETICON, ICON_BIG, (LPARAM)big_ico);

		//main message loop ...
		return app.run();
    }
    return 0;
}


