# Tae_Lee
Simulation Project
# Project 1: Simulation

{
   "cell_type": "code",
   "execution_count": 1,
   "metadata": {},
   "outputs": [],
   "source": [
    "#| echo: false\n",
    "\n",
    "# Fix for the progress bar not displaying correctly during training\n",
    "from IPython.display import clear_output, DisplayHandle\n",
    "\n",
    "\n",
    "def update_patch(self, obj):\n",
    "    clear_output(wait=True)\n",
    "    self.display(obj)\n",
    "\n",
    "\n",
    "DisplayHandle.update = update_patch\n"
   ]
  }
