# University of Washington, Programming Languages, Homework 6, hw6runner.rb

# This is the only file you turn in, so do not modify the other files as
# part of your solution.

class MyPiece < Piece
  # The constant All_My_Pieces should be declared here
  All_My_Pieces = [[[[0, 0], [1, 0], [0, 1], [1, 1]]],  # square (only needs one)
                rotations([[0, 0], [-1, 0], [1, 0], [0, -1]]), # T
                [[[0, 0], [-1, 0], [1, 0], [2, 0]], # long (only needs two)
                 [[0, 0], [0, -1], [0, 1], [0, 2]]],
                rotations([[0, 0], [0, -1], [0, 1], [1, 1]]), # L
                rotations([[0, 0], [0, -1], [0, 1], [-1, 1]]), # inverted L
                rotations([[0, 0], [-1, 0], [0, -1], [1, -1]]), # S
                rotations([[0, 0], [1, 0], [0, -1], [-1, -1]]), # Z
                rotations([[0, 0], [0, 1], [1, 0], [1, 1], [2, 0]]), # new piece 1
                rotations([[0, 0], [1, 0], [2, 0], [3, 0], [4, 0]]), # new piece 2
                rotations([[0, 0], [0, 1], [1, 0]])] # new piece 3
  # your enhancements here
  # class method to choose the next piece
  def self.next_piece (board)
    MyPiece.new(All_My_Pieces.sample, board)
  end
end

class MyBoard < Board
  # your enhancements here
  # rotates the current piece 180 degrees
  def rotate_180
    if !game_over? and @game.is_running?
      @current_block.move(0, 0, 2)
    end
    draw
  end

  def next_piece
    if @cheating
      @current_block = MyPiece.new([[[0, 0]]], self)
      @cheating = false # Reset @cheating after drawing the cheat piece
    else
      @current_block = MyPiece.next_piece(self)
    end
    @current_pos = nil
  end

  def cheat
    if @game.is_running? and @score >= 100 and !@cheating
      @score -= 100
      @cheating = true
    end
  end

  # new store_current where locations is not fixed to 0..3
  def store_current
    locations = @current_block.current_rotation
    displacement = @current_block.position
    (0..(locations.length - 1)).each{|index| 
      current = locations[index];
      @grid[current[1]+displacement[1]][current[0]+displacement[0]] = 
        @current_pos[index]
    }
    remove_filled
    @delay = [@delay - 2, 80].max
  end
end

class MyTetris < Tetris
  # your enhancements here
  def set_board
    @canvas = TetrisCanvas.new
    @board = MyBoard.new(self)
    @canvas.place(@board.block_size * @board.num_rows + 3,
                  @board.block_size * @board.num_columns + 6, 24, 80)
    @board.draw
  end

  def key_bindings
    super
    @root.bind('u', proc {@board.rotate_180})
    @root.bind('c', proc {@board.cheat})
  end

  def buttons
    super
    rotate_180 = TetrisButton.new('180', 'lightgreen'){@board.rotate_180}
    rotate_180.place(35, 50, 130, 501)
    cheat = TetrisButton.new('cheat', 'lightgreen'){@board.cheat}
    cheat.place(35, 60, 130, 570)
  end
end
