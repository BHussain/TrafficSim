using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Microsoft.Xna.Framework;
using Microsoft.Xna.Framework.Graphics;

namespace ConsoleApplication2
{
    class Game : Microsoft.Xna.Framework.Game
    {
        SpriteBatch spriteBatch;
        GraphicsDeviceManager graphics;
        TrafficSimulation.GameState gameState;

        public Game()
        {
            graphics = new GraphicsDeviceManager(this);
            Content.RootDirectory = "Content";
        }

        protected override void LoadContent()
        {
            spriteBatch = new SpriteBatch(GraphicsDevice);
            gameState = TrafficSimulation.InitialState();
            base.LoadContent();
        }

        protected override void Update(GameTime gameTime)
        {
            gameState = TrafficSimulation.UpdateState(
              (float)gameTime.ElapsedGameTime.TotalSeconds,
              gameState);
            base.Update(gameTime);
        }

        protected override void Draw(GameTime gameTime)
        {
            //Console.WriteLine("Moveable is a car? "+gameState.Moveable.MoveableType.IsCar+", Position: "+gameState.Moveable.Position);
            Console.WriteLine(gameState.Moveables.IsEmpty);
            GraphicsDevice.Clear(Color.LightGray);

            spriteBatch.Begin();
            foreach (var drawable in TrafficSimulation.drawState(gameState))
            {
                spriteBatch.Draw(Content.Load<Texture2D>(drawable.Image), 
                    new Vector2(drawable.Position.Item1, drawable.Position.Item2), 
                    Color.White);
            }
            spriteBatch.End();

            base.Draw(gameTime);
        }
    }
        class Program
    {
        static void Main(string[] args)
        {
            using (var game = new Game())
            {
                game.Run();
            }
        }
    }
}
