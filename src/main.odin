package main

import "core:fmt"
import "core:math"
import "core:math/linalg"
import sdl "vendor:sdl2"

SCREEN_SIZE :: [2]i32{1920, 1080}
WORLD_SIZE :: [2]i32{380, 180}

input_state: Input_State

Collider :: struct {
	position:   [2]f32,
	velocity:   [2]f32,
	rotation:   f32,
	dimensions: [2]f32,
	color:      [4]u8,
}

collider_new :: proc(dimensions: [2]u16, position: [2]f32 = {0, 0}, color: [4]u8) -> Collider {
	return Collider{position, {0, 0}, 0, {f32(dimensions.x), f32(dimensions.y)}, color}
}

move_towards :: proc(current, target, max_delta: f32) -> f32 {
	if math.abs(target - current) <= max_delta do return target
	return current + math.sign(target - current) * max_delta
}

collider_draw :: proc(collider: Collider, g_renderer: ^sdl.Renderer) {
	texture_color :=
		u32(collider.color.r) << 24 |
		u32(collider.color.g) << 16 |
		u32(collider.color.b) << 8 |
		u32(collider.color.a)

	rect_surface := sdl.CreateRGBSurface(
		0,
		i32(collider.dimensions.x),
		i32(collider.dimensions.y),
		32,
		0xFF000000,
		0x00FF0000,
		0x0000FF00,
		0x00000000,
	)
	sdl.FillRect(
		rect_surface,
		&sdl.Rect{0, 0, i32(collider.dimensions.x), i32(collider.dimensions.y)},
		texture_color,
	)
	texture := sdl.CreateTextureFromSurface(g_renderer, rect_surface)
	sdl.SetTextureBlendMode(texture, sdl.BlendMode.BLEND)
	sdl.SetTextureAlphaMod(texture, collider.color.a)

	collider_rect := sdl.FRect {
		x = collider.position.x,
		y = collider.position.y,
		w = f32(collider.dimensions.x),
		h = f32(collider.dimensions.y),
	}

	sdl.RenderCopyExF(
		g_renderer,
		texture,
		nil,
		&collider_rect,
		f64(collider.rotation),
		nil,
		sdl.RendererFlip.NONE,
	)
}

collider_draw_update :: proc(collider: ^Collider, g_renderer: ^sdl.Renderer) {
	MOVEMENT_ACCELERATION :: 0.0007
	ROTATION_AMOUNT :: 0.1
	FRICTION :: 0.00007
	MAX_SPEED :: 0.5

	if input_state.movement_up.ended_down && collider.velocity.y >= -MAX_SPEED do collider.velocity.y -= MOVEMENT_ACCELERATION
	if input_state.movement_down.ended_down && collider.velocity.y <= MAX_SPEED do collider.velocity.y += MOVEMENT_ACCELERATION
	if input_state.movement_right.ended_down && collider.velocity.x <= MAX_SPEED do collider.velocity.x += MOVEMENT_ACCELERATION
	if input_state.movement_left.ended_down && collider.velocity.x >= -MAX_SPEED do collider.velocity.x -= MOVEMENT_ACCELERATION
	if input_state.rotate_clockwise.ended_down do collider.rotation -= ROTATION_AMOUNT
	if !(input_state.movement_up.ended_down || input_state.movement_down.ended_down) do collider.velocity.x = move_towards(collider.velocity.x, 0, FRICTION)
	if !(input_state.movement_left.ended_down || input_state.movement_right.ended_down) do collider.velocity.y = move_towards(collider.velocity.y, 0, FRICTION)
	if input_state.rotate_counter_clockwise.ended_down do collider.rotation += ROTATION_AMOUNT

	collider.position += collider.velocity

	collider_draw(collider^, g_renderer)
}

Input_State :: struct {
	movement_up:              Input,
	movement_right:           Input,
	movement_down:            Input,
	movement_left:            Input,
	rotate_clockwise:         Input,
	rotate_counter_clockwise: Input,
}

input_state_clear :: proc(input_state: ^Input_State) {
	input_state.movement_up.half_transitions = 0
	input_state.movement_right.half_transitions = 0
	input_state.movement_down.half_transitions = 0
	input_state.movement_left.half_transitions = 0
	input_state.rotate_clockwise.half_transitions = 0
	input_state.rotate_counter_clockwise.half_transitions = 0
}

Input :: struct {
	half_transitions: u32,
	ended_down:       bool,
}

main :: proc() {
	// Initialize SDL
	if init_res := sdl.Init(sdl.INIT_VIDEO); init_res < 0 {
		fmt.eprintfln("SDL could not initialize! SDL_Error: %v\n", sdl.GetError())
		return
	}
	defer sdl.Quit()

	// Create window
	g_window: ^sdl.Window
	if g_window = sdl.CreateWindow(
		"SDL Tutorial",
		sdl.WINDOWPOS_UNDEFINED,
		sdl.WINDOWPOS_UNDEFINED,
		SCREEN_SIZE.x,
		SCREEN_SIZE.y,
		sdl.WINDOW_SHOWN,
	); g_window == nil {
		fmt.printf("Window could not be created! SDL_Error: %s\n", sdl.GetError())
		return
	}
	defer sdl.DestroyWindow(g_window)

	g_renderer: ^sdl.Renderer
	if g_renderer = sdl.CreateRenderer(g_window, -1, sdl.RENDERER_ACCELERATED); g_renderer == nil {
		fmt.eprintfln("Renderer could not be created! SDL error: %v", sdl.GetError())
	}
	defer sdl.DestroyRenderer(g_renderer)

	sdl.SetRenderDrawBlendMode(g_renderer, sdl.BlendMode.BLEND)
	sdl.SetHint(sdl.HINT_RENDER_SCALE_QUALITY, "0")

	world_target := sdl.CreateTexture(
		g_renderer,
		sdl.PixelFormatEnum.RGBA32,
		sdl.TextureAccess.TARGET,
		WORLD_SIZE.x,
		WORLD_SIZE.y,
	)

	collider := collider_new({16, 16}, {20, 20}, {0x7F, 0xB2, 0x85, 0x9F})
	collider_2 := collider_new({40, 40}, {100, 100}, {0x7D, 0x83, 0xFF, 0x9F})

	quit := false
	for !quit {
		input_state_clear(&input_state)
		for e: sdl.Event; sdl.PollEvent(&e); {
			#partial switch e.type {
			case .QUIT:
				quit = true
			case .KEYDOWN, .KEYUP:
				if e.key.repeat != 0 do continue
				#partial switch e.key.keysym.scancode {
				case .W:
					input_state.movement_up.half_transitions += 1
					input_state.movement_up.ended_down = e.key.type == sdl.EventType.KEYDOWN
				case .D:
					input_state.movement_right.half_transitions += 1
					input_state.movement_right.ended_down = e.key.type == sdl.EventType.KEYDOWN
				case .S:
					input_state.movement_down.half_transitions += 1
					input_state.movement_down.ended_down = e.key.type == sdl.EventType.KEYDOWN
				case .A:
					input_state.movement_left.half_transitions += 1
					input_state.movement_left.ended_down = e.key.type == sdl.EventType.KEYDOWN
				case .Q:
					input_state.rotate_clockwise.half_transitions += 1
					input_state.rotate_clockwise.ended_down = e.key.type == sdl.EventType.KEYDOWN
				case .E:
					input_state.rotate_counter_clockwise.half_transitions += 1
					input_state.rotate_counter_clockwise.ended_down =
						e.key.type == sdl.EventType.KEYDOWN
				}
			}
		}

		sdl.SetRenderTarget(g_renderer, world_target)
		sdl.SetRenderDrawColor(g_renderer, 0xFF, 0xFF, 0xFF, 0xFF)
		sdl.RenderClear(g_renderer)
		num_contacts, contacts := collider_collide(collider, collider_2)

		collider_draw_update(&collider, g_renderer)
		collider_draw(collider_2, g_renderer)

		sdl.SetRenderDrawColor(g_renderer, 0x00, 0x00, 0x00, 0xFF)
		points: [2]sdl.FPoint
		if num_contacts == 2 {
			sdl.RenderDrawLineF(
				g_renderer,
				contacts[0].position.x,
				contacts[0].position.y,
				contacts[1].position.x,
				contacts[1].position.y,
			)
		}
		sdl.SetRenderDrawColor(g_renderer, 0xEF, 0x47, 0x6F, 0xFF)
		for i in 0 ..< num_contacts {
			contact := contacts[i]
			sdl.RenderDrawPointF(g_renderer, contact.position.x, contact.position.y)
		}

		sdl.SetRenderTarget(g_renderer, nil)
		sdl.SetRenderDrawColor(g_renderer, 0xFF, 0xFF, 0xFF, 0xFF)
		sdl.RenderClear(g_renderer)

		// Draw camera texture
		pixel_h := f32(SCREEN_SIZE.y) / f32(WORLD_SIZE.y)

		dst := sdl.Rect {
			x = 0,
			y = 0,
			w = WORLD_SIZE.x * i32(pixel_h),
			h = WORLD_SIZE.y * i32(pixel_h),
		}
		sdl.RenderCopy(g_renderer, world_target, nil, &dst)
		sdl.RenderPresent(g_renderer)
	}
}
// Box vertex and edge numbering:
//        e1
//   v2 ------ v1
//    |        |
// e2 |        | e4  --> x
//    |        |
//   v3 ------ v4
//        e3
//        |
//        v y

Contact :: struct {
	position:                  [2]f32,
	normal:                    [2]f32,
	r1, r2:                    [2]f32,
	separation:                f32,
	// Accumulated normal impulse
	pn:                        f32,
	// Accumulated tangent impulse
	pt:                        f32,
	// Accumulated normal impulse for position bias
	pnb:                       f32,
	mass_normal, mass_tangent: f32,
	bias:                      f32,
	feature_pair:              Feature_Pair,
}

Axis :: enum {
	Face_A_X,
	Face_A_Y,
	Face_B_X,
	Face_B_Y,
}

Edge :: enum {
	No_Edge,
	Edge1,
	Edge2,
	Edge3,
	Edge4,
}

Feature_Pair :: struct {
	in_Edge1:  Edge,
	out_Edge1: Edge,
	in_Edge2:  Edge,
	out_Edge2: Edge,
	value:     i32,
}

feature_pair_flip :: proc(fp: Feature_Pair) -> (new_fp: Feature_Pair) {
	new_fp.in_Edge1, new_fp.in_Edge2 = fp.in_Edge2, fp.in_Edge1
	new_fp.out_Edge1, new_fp.out_Edge2 = fp.out_Edge2, fp.out_Edge1
	return
}

Clip_Vertex :: struct {
	vec: [2]f32,
	fp:  Feature_Pair,
}

clip_segment_to_line :: proc(
	vertex_in: [2]Clip_Vertex,
	normal: [2]f32,
	offset: f32,
	clip_edge: Edge,
) -> (
	vertex_out: [2]Clip_Vertex,
	num_out: i32,
) {
	num_out = 0

	distance0 := linalg.dot(normal, vertex_in[0].vec) - offset
	distance1 := linalg.dot(normal, vertex_in[1].vec) - offset

	if distance0 <= 0.0 {
		vertex_out[num_out] = vertex_in[0]
		num_out += 1
	}
	if distance1 <= 0.0 {
		vertex_out[num_out] = vertex_in[1]
		num_out += 1
	}

	if distance0 * distance1 < 0.0 {
		interp := distance0 / (distance0 - distance1)
		vertex_out[num_out].vec = vertex_in[0].vec + interp * (vertex_in[1].vec - vertex_in[0].vec)
		if distance0 > 0.0 {
			vertex_out[num_out].fp = vertex_in[0].fp
			vertex_out[num_out].fp.in_Edge1 = clip_edge
			vertex_out[num_out].fp.in_Edge2 = .No_Edge
		} else {
			vertex_out[num_out].fp = vertex_in[1].fp
			vertex_out[num_out].fp.out_Edge1 = clip_edge
			vertex_out[num_out].fp.out_Edge2 = .No_Edge
		}
		num_out += 1
	}
	return
}

compute_incident_edge :: proc(
	half_size, position, normal: ^[2]f32,
	rot: matrix[2, 2]f32,
) -> (
	clip_vertex: [2]Clip_Vertex,
) {
	rot_t := linalg.transpose(rot)
	incident_normal := -(rot_t * normal^)
	incident_normal_abs := linalg.abs(incident_normal)

	if incident_normal_abs.x > incident_normal_abs.y {
		if math.sign(incident_normal.x) > 0.0 {
			clip_vertex[0].vec = {half_size.x, half_size.y}
			clip_vertex[0].fp.in_Edge2 = .Edge3
			clip_vertex[0].fp.out_Edge2 = .Edge4

			clip_vertex[1].vec = {half_size.x, -half_size.y}
			clip_vertex[1].fp.in_Edge2 = .Edge4
			clip_vertex[1].fp.out_Edge2 = .Edge1
		} else {
			clip_vertex[0].vec = {-half_size.x, -half_size.y}
			clip_vertex[0].fp.in_Edge2 = .Edge1
			clip_vertex[0].fp.out_Edge2 = .Edge2

			clip_vertex[1].vec = {-half_size.x, half_size.y}
			clip_vertex[1].fp.in_Edge2 = .Edge2
			clip_vertex[1].fp.out_Edge2 = .Edge3
		}
	} else {
		if math.sign(incident_normal.y) > 0.0 {
			clip_vertex[0].vec = {half_size.x, half_size.y}
			clip_vertex[0].fp.in_Edge2 = .Edge4
			clip_vertex[0].fp.out_Edge2 = .Edge3

			clip_vertex[1].vec = {-half_size.x, half_size.y}
			clip_vertex[1].fp.in_Edge2 = .Edge3
			clip_vertex[1].fp.out_Edge2 = .Edge2
		} else {
			clip_vertex[0].vec = {-half_size.x, -half_size.y}
			clip_vertex[0].fp.in_Edge2 = .Edge2
			clip_vertex[0].fp.out_Edge2 = .Edge1

			clip_vertex[1].vec = {half_size.x, -half_size.y}
			clip_vertex[1].fp.in_Edge2 = .Edge1
			clip_vertex[1].fp.out_Edge2 = .Edge4
		}
	}
	clip_vertex[0].vec = position^ + rot * clip_vertex[0].vec
	clip_vertex[1].vec = position^ + rot * clip_vertex[1].vec
	return
}

collider_collide :: proc(
	collider_a, collider_b: Collider,
) -> (
	num_contacts: u32,
	contacts: [2]Contact,
) {
	num_contacts = 0

	half_a_size := collider_a.dimensions / 2
	half_b_size := collider_b.dimensions / 2

	a_pos := collider_a.position + half_a_size
	b_pos := collider_b.position + half_b_size

	a_rot := linalg.matrix2_rotate_f32(math.to_radians(collider_a.rotation))
	b_rot := linalg.matrix2_rotate_f32(math.to_radians(collider_b.rotation))

	a_rot_transpose := linalg.transpose(a_rot)
	b_rot_transpose := linalg.transpose(b_rot)

	displacement := b_pos - a_pos
	displacement_a := displacement * a_rot_transpose
	displacement_b := displacement * b_rot_transpose

	t := linalg.matrix_flatten(a_rot_transpose * b_rot)
	rotation_b_to_a := matrix[2, 2]f32{
		abs(t[0]), abs(t[1]), 
		abs(t[2]), abs(t[3]), 
	}
	fmt.printfln("t %v r %v", t, rotation_b_to_a)
	rotation_a_to_b := linalg.transpose(rotation_b_to_a)

	// Box A Faces
	face_a := linalg.abs(displacement_a) - half_a_size - rotation_b_to_a * displacement_b
	if face_a.x > 0.0 || face_a.y > 0.0 do return

	face_b := linalg.abs(displacement_b) - half_b_size - rotation_a_to_b * displacement_a
	if face_b.x > 0.0 || face_b.y > 0.0 do return

	axis := Axis.Face_A_X
	separation := face_a.x
	// Column 1
	normal := [2]f32{a_rot[0, 0], a_rot[1, 0]} * (displacement_a.x > 0.0 ? 1 : -1)

	RELATIVE_TOL :: 0.95
	ABSOLUTE_TOL :: 0.01

	if face_a.y > RELATIVE_TOL * separation + ABSOLUTE_TOL * half_a_size.y {
		axis = .Face_A_Y
		separation = face_a.y
		// Column 2
		normal = [2]f32{a_rot[0, 1], a_rot[1, 1]} * (displacement_a.y > 0.0 ? 1 : -1)
	}

	// Box B Faces
	if face_b.x > RELATIVE_TOL * separation + ABSOLUTE_TOL * half_b_size.x {
		axis = .Face_B_X
		separation = face_b.x
		// Column 1
		normal = [2]f32{b_rot[0, 0], b_rot[1, 0]} * (displacement_b.x > 0 ? 1 : -1)
	}

	if face_b.y > RELATIVE_TOL * separation + ABSOLUTE_TOL * half_b_size.y {
		axis = .Face_B_Y
		separation = face_b.y
		// Column 2
		normal = [2]f32{b_rot[1, 0], b_rot[1, 1]} * (displacement_b.y > 0 ? 1 : -1)
	}

	front_normal, side_normal: [2]f32
	incident_edge: [2]Clip_Vertex
	front, neg_side, pos_side: f32
	neg_edge, pos_edge: Edge

	switch axis {
	case .Face_A_X:
		front_normal = normal
		front = linalg.dot(a_pos, front_normal) + half_a_size.x
		side_normal = {a_rot[1, 0], a_rot[1, 1]}
		side := linalg.dot(a_pos, side_normal)
		neg_side = -side + half_a_size.y
		pos_side = side + half_a_size.y
		neg_edge = .Edge1
		pos_edge = .Edge3
		incident_edge = compute_incident_edge(&half_b_size, &b_pos, &front_normal, b_rot)

	case .Face_A_Y:
		front_normal = normal
		front = linalg.dot(a_pos, front_normal) + half_a_size.y
		side_normal = {a_rot[0, 0], a_rot[1, 0]}
		side := linalg.dot(a_pos, side_normal)
		neg_side = -side + half_a_size.x
		pos_side = side + half_a_size.x
		neg_edge = .Edge2
		pos_edge = .Edge4
		incident_edge = compute_incident_edge(&half_b_size, &b_pos, &front_normal, b_rot)

	case .Face_B_X:
		front_normal = -normal
		front = linalg.dot(b_pos, front_normal) + half_b_size.x
		side_normal = {b_rot[0, 1], b_rot[1, 1]}
		side := linalg.dot(b_pos, side_normal)
		neg_side = -side + half_b_size.y
		pos_side = side + half_b_size.y
		neg_edge = .Edge1
		pos_edge = .Edge3
		incident_edge = compute_incident_edge(&half_a_size, &a_pos, &front_normal, a_rot)

	case .Face_B_Y:
		front_normal = -normal
		front = linalg.dot(b_pos, front_normal) + half_b_size.y
		side_normal = {b_rot[0, 0], b_rot[1, 0]}
		side := linalg.dot(b_pos, side_normal)
		neg_side = -side + half_b_size.x
		pos_side = side + half_b_size.x
		neg_edge = .Edge2
		pos_edge = .Edge4
		incident_edge = compute_incident_edge(&half_a_size, &a_pos, &front_normal, a_rot)
	}

	clip_points1, np1 := clip_segment_to_line(incident_edge, -side_normal, neg_side, neg_edge)
	if np1 < 2 do return

	clip_points2, np2 := clip_segment_to_line(clip_points1, side_normal, pos_side, pos_edge)
	if np2 < 2 do return

	for i in 0 ..< 2 {
		separation := linalg.dot(front_normal, clip_points2[i].vec) - front
		if separation > 0 do continue

		contacts[num_contacts].separation = separation
		contacts[num_contacts].normal = normal
		// slide contact point onto reference face (easy to cull)
		contacts[num_contacts].position = clip_points2[i].vec - separation * front_normal
		contacts[num_contacts].feature_pair = clip_points2[i].fp
		if axis == .Face_B_X || axis == .Face_B_Y {
			contacts[num_contacts].feature_pair = feature_pair_flip(
				contacts[num_contacts].feature_pair,
			)
		}
		num_contacts += 1
	}
	return
}
