using System;
using System.Collections.Generic;
using System.Linq;
using System.Text;
using System.Threading.Tasks;
using Microsoft.Xna.Framework;

namespace ConsoleApplication2
{
    class Game : Microsoft.Xna.Framework.Game
    {
        GraphicsDeviceManager graphics;
        TrafficSimulation.GameState gameState;

        public Game()
        {
            graphics = new GraphicsDeviceManager(this);
            Content.RootDirectory = "Content";
        }

        protected override void LoadContent()
        {
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
            //Print de huidige waarde van de stoplicht float
            //Console.WriteLine(gameState.TrafficLight.Color);
            //Console.WriteLine(gameState.Child.Position);
            Console.WriteLine(gameState.Car.Position);

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
